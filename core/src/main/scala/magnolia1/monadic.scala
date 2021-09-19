package magnolia1

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

trait Functor[F[_]] {
  type Apply[X] = F[X]
  def point[A](value: A): F[A]
  def map[A, B](from: F[A])(fn: A => B): F[B]
}

trait Monadic[F[_]] extends Functor[F] {
  def flatMap[A, B](from: F[A])(fn: A => F[B]): F[B]
}

object Monadic {

  implicit object monadicOpt extends Monadic[Option] {
    def point[A](value: A): Option[A] = Some(value)
    def map[A, B](from: Option[A])(fn: A => B): Option[B] = from.map(fn)
    def flatMap[A, B](from: Option[A])(fn: A => Option[B]): Option[B] = from.flatMap(fn)
  }

  implicit object monadicList extends Monadic[List] {
    def point[A](value: A): List[A] = List(value)
    def map[A, B](from: List[A])(fn: A => B): List[B] = from.map(fn)
    def flatMap[A, B](from: List[A])(fn: A => List[B]): List[B] = from.flatMap(fn)
  }

  class monadicFuture(implicit ec: ExecutionContext) extends Monadic[Future] {
    def point[A](value: A): Future[A] = Future(value)
    def map[A, B](from: Future[A])(fn: A => B): Future[B] = from.map(fn)
    def flatMap[A, B](from: Future[A])(fn: A => Future[B]): Future[B] = from.flatMap(fn)
  }

  final implicit def monadicFuture(implicit ec: ExecutionContext): monadicFuture = new monadicFuture()

  class monadicEither[Err] extends Monadic[Either[Err, *]] {
    def point[A](value: A): Either[Err, A] = Right(value)
    def map[A, B](from: Either[Err, A])(fn: A => B): Either[Err, B] = from.map(fn)
    def flatMap[A, B](from: Either[Err, A])(fn: A => Either[Err, B]): Either[Err, B] = from.flatMap(fn)
  }

  final implicit def monadicEither[Err]: monadicEither[Err] = new monadicEither[Err]

  implicit object monadicTry extends Monadic[Try] {
    def point[A](value: A): Try[A] = Success(value)
    def map[A, B](from: Try[A])(fn: A => B): Try[B] = from.map(fn)
    def flatMap[A, B](from: Try[A])(fn: A => Try[B]): Try[B] = from.flatMap(fn)
  }

  final implicit class Ops[F[_], A](val fv:F[A]) extends AnyVal {
    def flatMap[B](f: A => F[B])(implicit monadic: Monadic[F]): F[B] = monadic.flatMap(fv)(f)
    def map[B](f: A => B)(implicit functor: Functor[F]): F[B] = functor.map(fv)(f)
  }
}
