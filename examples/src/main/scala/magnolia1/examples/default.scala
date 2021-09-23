package magnolia1.examples

import magnolia1.{CaseClass, Magnolia, SealedTrait}

import scala.language.experimental.macros

/** typeclass for providing a default value for a particular type */
trait HasDefault[T] { def defaultValue: Either[String, T] }

/** companion object and derivation object for [[HasDefault]] */
object HasDefault {

  type Typeclass[T] = HasDefault[T]

  /** constructs a default for each parameter, using the constructor default (if provided), otherwise using a typeclass-provided default
    */
  def join[T](ctx: CaseClass[HasDefault, T]): HasDefault[T] = new HasDefault[T] {
    def defaultValue = ctx.constructMonadic { param =>
      param.default match {
        case Some(arg) => Right(arg)
        case None      => param.typeclass.defaultValue
      }
    }
  }

  /** chooses which subtype to delegate to */
  def split[T](ctx: SealedTrait[HasDefault, T])(): HasDefault[T] = new HasDefault[T] {
    def defaultValue = ctx.subtypes.headOption match {
      case Some(sub) => sub.typeclass.defaultValue
      case None      => Left("no subtypes")
    }
  }

  /** default value for a string; the empty string */
  implicit val string: HasDefault[String] = new HasDefault[String] { def defaultValue = Right("") }

  /** default value for ints; 0 */
  implicit val int: HasDefault[Int] = new HasDefault[Int] { def defaultValue = Right(0) }

  /** oh, no, there is no default Boolean... whatever will we do? */
  implicit val boolean: HasDefault[Boolean] = new HasDefault[Boolean] { def defaultValue = Left("truth is a lie") }

  /** default value for sequences; the empty sequence */
  implicit def seq[A]: HasDefault[Seq[A]] = new Typeclass[Seq[A]] { def defaultValue = Right(Seq.empty) }

  /** generates default instances of [[HasDefault]] for case classes and sealed traits */
  implicit def gen[T]: HasDefault[T] = macro Magnolia.gen[T]
}
