package magnolia1

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success}

trait Monadic[F[_]]:
  type Apply[X] = F[X]
  def point[A](value: A): F[A]
  def map[A, B](from: F[A])(fn: A => B): F[B]
  def flatMap[A, B](from: F[A])(fn: A => F[B]): F[B]

object Monadic:
  given Monadic[Option] with
    def point[A](value: A): Option[A] = Some(value)
    def map[A, B](from: Option[A])(fn: A => B): Option[B] = from.map(fn)
    def flatMap[A, B](from: Option[A])(fn: A => Option[B]): Option[B] = from.flatMap(fn)

  given Monadic[List] with
    def point[A](value: A): List[A] = List(value)
    def map[A, B](from: List[A])(fn: A => B): List[B] = from.map(fn)
    def flatMap[A, B](from: List[A])(fn: A => List[B]): List[B] = from.flatMap(fn)

  given (using ec: ExecutionContext): Monadic[Future] with
    def point[A](value: A): Future[A] = Future(value)
    def map[A, B](from: Future[A])(fn: A => B): Future[B] = from.map(fn)
    def flatMap[A, B](from: Future[A])(fn: A => Future[B]): Future[B] = from.flatMap(fn)

  given [Err]: Monadic[[X] =>> Either[Err, X]] with
    def point[A](value: A): Either[Err, A] = Right(value)
    def map[A, B](from: Either[Err, A])(fn: A => B): Either[Err, B] = from.map(fn)
    def flatMap[A, B](from: Either[Err, A])(fn: A => Either[Err, B]): Either[Err, B] = from.flatMap(fn)

  given Monadic[Try] with
    def point[A](value: A): Try[A] = Success(value)
    def map[A, B](from: Try[A])(fn: A => B): Try[B] = from.map(fn)
    def flatMap[A, B](from: Try[A])(fn: A => Try[B]): Try[B] = from.flatMap(fn)

  extension [F[_], A, B](fv: F[A])(using monadic: Monadic[F])
    def map(f: A => B): F[B] = monadic.map(fv)(f)
    def flatMap(f: A => F[B]): F[B] = monadic.flatMap(fv)(f)