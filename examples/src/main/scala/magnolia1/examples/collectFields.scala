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

  type Typeclass[A] = CollectFields[Any, A]

  implicit val int: CollectFields[IntField, Int] = int => Seq(IntField(int))
  implicit val string: CollectFields[StringField, String] = string => Seq(StringField(string))

  implicit def gen[A]: CollectFields[Any, A] = macro Magnolia.gen[A]

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
