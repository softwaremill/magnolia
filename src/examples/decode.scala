package magnolia2.examples

import magnolia2.*

/** very basic decoder for converting strings to other types */
trait Decoder[T]:
  def decode(str: String): T

/** derivation object (and companion object) for [[Decoder]] instances */
object Decoder extends AutoDerivation[Decoder]:

  given Decoder[String] = (s: String) => s
  given Decoder[Int] = _.toInt

  /** defines how new [[Decoder]]s for case classes should be constructed */
  def join[T](ctx: CaseClass[Decoder, T]): Decoder[T] = value =>
    val (_, values) = parse(value)
    ctx.construct { param =>
      values
        .get(param.label)
        .map(param.typeclass.decode)
        .orElse(param.default)
        .getOrElse(sys.error(s"missing ${param.label}"))
    }

  /** defines how to choose which subtype of the sealed trait to use for
    * decoding
    */
  override def split[T](ctx: SealedTrait[Decoder, T]): Decoder[T] = param =>
    val (name, _) = parse(param)
    val subtype = ctx.subtypes.find(_.typeInfo.full == name).get

    subtype.typeclass.decode(param)

  /** very simple extractor for grabbing an entire parameter value, assuming
    * matching parentheses
    */
  private def parse(value: String): (String, Map[String, String]) =
    val end = value.indexOf('(')
    val name = value.substring(0, end)

    def parts(
        value: String,
        idx: Int = 0,
        depth: Int = 0,
        collected: List[String] = List("")
    ): List[String] =
      def plus(char: Char): List[String] =
        collected.head + char :: collected.tail

      if (idx == value.length) collected
      else
        value(idx) match
          case '(' =>
            parts(value, idx + 1, depth + 1, plus('('))
          case ')' =>
            if depth == 1 then plus(')')
            else parts(value, idx + 1, depth - 1, plus(')'))
          case ',' =>
            if depth == 0 then parts(value, idx + 1, depth, "" :: collected)
            else parts(value, idx + 1, depth, plus(','))
          case char =>
            parts(value, idx + 1, depth, plus(char))

    def keyValue(str: String): (String, String) =
      val List(label, value) = str.split("=", 2).to(List)
      (label, value)

    (
      name,
      parts(value.substring(end + 1, value.length - 1))
        .filter(_.nonEmpty)
        .map(keyValue)
        .toMap
    )
