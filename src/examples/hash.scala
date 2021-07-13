package magnolia1.examples

import magnolia1.*

trait WeakHash[T]:
  def hash(value: T): Int

object WeakHash extends Derivation[WeakHash]:
  def join[T](ctx: CaseClass[WeakHash, T]): WeakHash[T] = value =>
    ctx.params.map { param => param.typeclass.hash(param.deref(value)) }.foldLeft(0)(_ ^ _)

  override def split[T](ctx: SealedTrait[WeakHash, T]): WeakHash[T] = new WeakHash[T]:
    def hash(value: T): Int = ctx.choose(value) { sub => sub.typeclass.hash(sub.value) }

  given WeakHash[String] = _.map(_.toInt).sum
  given WeakHash[Int] = identity(_)
  given WeakHash[Double] = _.toInt