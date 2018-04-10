import io.circe.{Encoder, Json}
import magnolia._

import scala.language.experimental.macros

object MagnoliaEncoder {

  type Typeclass[T] = Encoder[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = new Encoder[T] {
    def apply(a: T): Json = {
      if (caseClass.isValueClass) {
        val p = caseClass.parameters.head
        p.typeclass(p.dereference(a))
      }
      else if (caseClass.isObject)
        Json.fromString(caseClass.typeName.short)
      else Json.obj(
        caseClass.parameters.map(p =>
          p.label -> p.typeclass(p.dereference(a))
        ): _*
      )
    }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = new Encoder[T] {
    def apply(a: T): Json =
      sealedTrait.dispatch(a) { subtype =>
        Json.obj(
          subtype.typeName.short -> subtype.typeclass(subtype.cast(a))
        )
      }
  }

  implicit def genEncoder[T]: Typeclass[T] = macro Magnolia.gen[T]
}
