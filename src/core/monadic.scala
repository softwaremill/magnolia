/*

    Magnolia, version 0.17.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package magnolia

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success}

trait Functor[F[_]]:
  type Apply[X] = F[X]
  def point[A](value: A): F[A]
  def map[A, B](from: F[A])(fn: A => B): F[B]

trait Monadic[F[_]] extends Functor[F]:
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

  extension [F[_], A, B](fv: F[A])(using functor: Functor[F]) def map(f: A => B): F[B] = functor.map(fv)(f)

  extension [F[_], A, B](fv: F[A])(using monadic: Monadic[F])
    def flatMap(f: A => F[B]): F[B] = monadic.flatMap(fv)(f)