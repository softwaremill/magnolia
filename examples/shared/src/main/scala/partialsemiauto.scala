package magnolia.examples

import java.util.UUID
import scala.language.experimental.macros
import magnolia._

import scala.annotation.StaticAnnotation

trait PartialSemiDefault[A] {
  def default: A
}

case class ForceProvidedDefault() extends StaticAnnotation with ForceFallbackDerivation
case class NoDefault() extends StaticAnnotation with ForceFallbackDerivation

object PartialSemiDefault {
  @inline def apply[A](implicit A: PartialSemiDefault[A]): PartialSemiDefault[A] = A

  type Typeclass[T] = PartialSemiDefault[T]

  def fallback[T]: PartialSemiDefault[T] = ??? // this will never be called because of how combine/dispatch is implemented

  def combine[T](ctx: CaseClass[PartialSemiDefault, T]): PartialSemiDefault[T] = new PartialSemiDefault[T] {
    def default = ctx.construct { p =>
      p.default.getOrElse {
        if (p.annotations.collectFirst {
          case ForceProvidedDefault() => true
        }.nonEmpty) {
          throw new IllegalStateException(s"ForceProvidedDefault on $p but no explicit default provided")
        } else {
          p.typeclass.default
        }
      }
    }
  }
  def dispatch[T](ctx: SealedTrait[PartialSemiDefault, T])(): PartialSemiDefault[T] = new PartialSemiDefault[T] {
    def default =
      ctx.subtypes
        .filterNot { subtype =>
          subtype.annotations.exists {
            case NoDefault() => true
            case _ => false
          }
        }
        .head
        .typeclass
        .default
  }

  implicit val string: PartialSemiDefault[String] = new PartialSemiDefault[String] { def default = "" }
  implicit val int: PartialSemiDefault[Int] = new PartialSemiDefault[Int] { def default = 0 }

  def gen[T]: PartialSemiDefault[T] = macro Magnolia.gen[T]
}

object PartialSemiDefaultExample extends App {
  val fallbackUUID = UUID.fromString("00000000-0000-0000-0000-000000000000")

  sealed trait Data
  object Data {
    implicit val d1: PartialSemiDefault[Data] = PartialSemiDefault.gen
  }

  // Data1 is fully derivable
  case class Data1(a: Int = 10, b: String) extends Data
  object Data1 {
    implicit val d: PartialSemiDefault[Data1] = PartialSemiDefault.gen
  }

  // Data2's Double and UUID does not have an implicit typeclass instance. Even though we provide a default value,
  // without the annotation Magnolia would fail on implicit not found
  case class Data2(c: Data1, @ForceProvidedDefault d: Double = 3.14, @ForceProvidedDefault e: UUID = fallbackUUID) extends Data
  object Data2 {
    implicit val d: PartialSemiDefault[Data2] = PartialSemiDefault.gen
  }

  // Data3 is non derivable as it has no default for parameter f, and we don't have a default typeclass instance for UUID,
  // but by marking it with the annotation we can ignore it in dispatch
  @NoDefault case class Data3(f: UUID) extends Data
}