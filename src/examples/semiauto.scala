package magnolia.examples

import scala.language.experimental.macros
import magnolia._

trait SemiDefault[A]:
  def default: A

object SemiDefault extends Derivation[SemiDefault]:
  inline def apply[A](using A: SemiDefault[A]): SemiDefault[A] = A

  def join[T](ctx: CaseClass[SemiDefault, T]): SemiDefault[T] = new SemiDefault[T]:
    def default = ctx.construct(p => p.default.getOrElse(p.typeclass.default))

  override def split[T](ctx: SealedTrait[SemiDefault, T]): SemiDefault[T] = new SemiDefault[T]:
    def default = ctx.subtypes.head.typeclass.default
  
  given SemiDefault[String] with
    def default = ""
  
  given SemiDefault[Int] with
    def default = 0