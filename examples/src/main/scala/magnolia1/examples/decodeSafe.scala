package magnolia1.examples

import magnolia1.{CaseClass, Magnolia, SealedTrait}

import scala.language.experimental.macros

/** decoder for converting strings to other types providing good error messages */
trait DecoderSafe[T] { def decode(str: String): Either[String, T] }

/** derivation object (and companion object) for [[DecoderSafe]] instances */
object DecoderSafe {

  /** decodes strings */
  implicit val string: DecoderSafe[String] = (s: String) => Right(s)

  /** decodes ints */
  implicit val int: DecoderSafe[Int] = { k =>
    try Right(k.toInt)
    catch { case _: NumberFormatException => Left(s"illegal number: $k") }
  }

  /** binds the Magnolia macro to this derivation object */
  implicit def gen[T]: DecoderSafe[T] = macro Magnolia.gen[T]

  /** type constructor for new instances of the typeclass */
  type Typeclass[T] = DecoderSafe[T]

  /** defines how new [[DecoderSafe]]s for case classes should be constructed */
  def combine[T](ctx: CaseClass[DecoderSafe, T]): DecoderSafe[T] = value => {
    val (_, values) = parse(value)
    ctx.constructEither { param =>
      param.typeclass.decode(values(param.label))
    }.left.map(_.reduce(_ + "\n" + _))
  }

  /** defines how to choose which subtype of the sealed trait to use for decoding */
  def dispatch[T](ctx: SealedTrait[DecoderSafe, T]): DecoderSafe[T] = param => {
    val (name, _) = parse(param)
    val subtype = ctx.subtypes.find(_.typeName.full == name).get
    subtype.typeclass.decode(param)
  }

  /** very simple extractor for grabbing an entire parameter value, assuming matching parentheses */
  private def parse(value: String): (String, Map[String, String]) = {
    val end = value.indexOf('(')
    val name = value.substring(0, end)

    def parts(value: String,
              idx: Int = 0,
              depth: Int = 0,
              collected: List[String] = List("")): List[String] = {
      def plus(char: Char): List[String] = collected.head + char :: collected.tail

      if (idx == value.length) collected
      else
        value(idx) match {
          case '(' =>
            parts(value, idx + 1, depth + 1, plus('('))
          case ')' =>
            if (depth == 1) plus(')')
            else parts(value, idx + 1, depth - 1, plus(')'))
          case ',' =>
            if (depth == 0) parts(value, idx + 1, depth, "" :: collected)
            else parts(value, idx + 1, depth, plus(','))
          case char =>
            parts(value, idx + 1, depth, plus(char))
        }
    }

    def keyValue(str: String): (String, String) = {
      val List(label, value) = str.split("=", 2).toList
      (label, value)
    }

    (name, parts(value.substring(end + 1, value.length - 1)).map(keyValue).toMap)
  }
}
