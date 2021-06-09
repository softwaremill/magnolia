package magnolia.examples

import magnolia.*

trait Eq[T]:
  def equal(value: T, value2: T): Boolean

object Eq extends AutoDerivation[Eq]:
  def join[T](ctx: CaseClass[Eq, T]): Eq[T] = (v1, v2) =>
    ctx.params.forall { p => p.typeclass.equal(p.deref(v1), p.deref(v2)) }

  override def split[T](ctx: SealedTrait[Eq, T]): Eq[T] = (v1, v2) => ctx.choose(v1) {
    sub => sub.typeclass.equal(sub.value, sub.cast(v2))
  }

  given Eq[String] = _ == _
  given Eq[Int] = _ == _

  given [T: Eq]: Eq[Option[T]] =
    case (Some(v1), Some(v2)) => summon[Eq[T]].equal(v1, v2)
    case (None, None)         => true
    case _                    => false

  given [T: Eq, C[x] <: Iterable[x]]: Eq[C[T]] = (v1, v2) =>
    v1.size == v2.size && (v1.iterator zip v2.iterator).forall((summon[Eq[T]].equal).tupled)