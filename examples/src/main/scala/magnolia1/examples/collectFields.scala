package magnolia1.examples

import magnolia1.ReadOnlyCaseClass
import magnolia1.SealedTrait
import magnolia1.Magnolia

trait CollectFields[+Out, A] {
  def collectFields(a: A): Seq[Out]
}

object CollectFields {

  trait Field
  case class IntField(int: Int) extends Field
  case class StringField(string: String) extends Field

  implicit val int: CollectFields[IntField, Int] = int => Seq(IntField(int))
  implicit val string: CollectFields[StringField, String] = string => Seq(StringField(string))
  implicit def seq[Out, A](implicit A: CollectFields[Out, A]): CollectFields[Out, Seq[A]] =
    _.flatMap(A.collectFields)
  implicit def option[Out, A](implicit A: CollectFields[Out, A]): CollectFields[Out, Option[A]] =
    _.fold(Seq.empty[Out])(A.collectFields)

  def instance[Out, A](f: A => Seq[Out]): CollectFields[Out, A] = f(_)
  def apply[A](implicit A: CollectFields[_, A]): A.type = A

  object genDerivation extends Derivation {
    type Typeclass[A] = CollectFields[Any, A]

    implicit def gen[A]: Typeclass[A] = macro Magnolia.gen[A]
  }

  object genNarrowDerivation extends Derivation {
    implicit def genNarrow[Tc[_] <: CollectFields[Any, _], A]: Tc[A] =
      macro Magnolia.genNarrow[Tc, A]
  }

  protected trait Derivation {
    def join[Out, A](caseClass: ReadOnlyCaseClass[CollectFields[Out, *], A]): CollectFields[Out, A] =
      new CollectFields[Out, A] {
        override def collectFields(a: A) = caseClass.parameters.flatMap { param =>
          param.typeclass.collectFields(
            param.dereference(a)
          )
        }
      }

    def split[Out, A](sealedTrait: SealedTrait[CollectFields[Out, *], A]): CollectFields[Out, A] =
      new CollectFields[Out, A] {
        override def collectFields(a: A) = sealedTrait.split(a) { subtype =>
          subtype.typeclass.collectFields(
            subtype.cast(a)
          )
        }
      }
  }
}
