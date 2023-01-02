package magnolia2.examples

import magnolia2.*

/** Type class for copying an instance of some type `T`, thereby replacing
  * certain fields with other values.
  */
sealed abstract class Patcher[T]:

  /** Returns a copy of `value` whereby all non-null elements of `fieldValues`
    * replace the respective fields of `value`. For all null elements of
    * `fieldValues` the original value of the respective field of `value` is
    * maintained.
    *
    * If the size of `fieldValues` doesn't exactly correspond to the number of
    * fields of `value` an [[IllegalArgumentException]] is thrown.
    */
  def patch(value: T, fieldValues: Seq[Any]): T

object Patcher extends LowerPriorityPatcher with Derivation[Patcher]:
  def join[T](ctx: CaseClass[Patcher, T]): Patcher[T] =
    new Patcher[T]:
      def patch(value: T, fieldValues: Seq[Any]): T =
        if fieldValues.lengthCompare(ctx.params.size) != 0 then
          throw new IllegalArgumentException(
            s"Cannot patch value `$value`, expected ${ctx.params.size} fields but got ${fieldValues.size}"
          )

        val effectiveFields = ctx.params.zip(fieldValues).map { (param, x) =>
          if (x.asInstanceOf[AnyRef] ne null) x else param.deref(value)
        }

        ctx.rawConstruct(effectiveFields)

  def split[T](ctx: SealedTrait[Patcher, T]): Patcher[T] = new Patcher[T]:
    def patch(value: T, fieldValues: Seq[Any]): T =
      ctx.choose(value)(sub => sub.typeclass.patch(sub.value, fieldValues))

sealed abstract class LowerPriorityPatcher:
  private[this] val _forSingleValue =
    new Patcher[Any]:
      def patch(value: Any, fieldValues: Seq[Any]): Any = {
        if (fieldValues.lengthCompare(1) != 0)
          throw new IllegalArgumentException(
            s"Cannot patch single value `$value` with patch sequence of size ${fieldValues.size}"
          )
        val head = fieldValues.head
        if (head.getClass != value.getClass)
          throw new IllegalArgumentException(
            s"Illegal patch value type. Expected `${value.getClass}` but got `${head.getClass}`"
          )
        head
      }

  given forSingleValue[T]: Patcher[T] = _forSingleValue.asInstanceOf[Patcher[T]]
