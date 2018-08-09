/* Mercator, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://propensive.com
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package mercator

import scala.annotation.compileTimeOnly
import scala.language.higherKinds
import scala.reflect.macros._

import language.experimental.macros

object `package` {
  implicit def monadicEvidence[F[_]]: Monadic[F] =
    macro Mercator.gen[F[Nothing]]
  
  final implicit class Ops[M[_], A](val value: M[A]) extends AnyVal {
    @inline def flatMap[B](fn: A => M[B])(implicit monadic: Monadic[M]): M[B] =
      monadic.flatMap[A, B](value, fn)

    @inline def map[B](fn: A => B)(implicit monadic: Monadic[M]): M[B] =
      monadic.map[A, B](value, fn)
  }
}

object Mercator {
  def gen[F: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val typeConstructor = weakTypeOf[F].typeConstructor

    val companion = typeConstructor.typeSymbol.companion
    lazy val methodParams = companion.asModule.info.member(TermName("apply")).typeSignature.paramLists
    val pointApplication = if(companion != NoSymbol && methodParams.length > 0 &&
        methodParams.head.length > 0) q"${companion.asModule}(value)"
    else {
      import internal._
      val genericType = appliedType(typeConstructor, typeOf[Mercator.type])
      val subtypes = typeConstructor.typeSymbol.asClass.knownDirectSubclasses.filter { sub =>
        val companion = sub.asType.toType.companion
        val attempt = c.typecheck(q"$companion.apply(_root_.mercator.Mercator)").tpe
        attempt <:< genericType
      }.map { sub => q"${sub.companion}.apply(value)" }
      if(subtypes.size == 1) subtypes.head else ???
    }

    q"""
      import scala.language.higherKinds
      new Monadic[$typeConstructor] {
        def point[A](value: A): Monad[A] = $pointApplication
      
        def flatMap[A, B](from: Monad[A], fn: A => Monad[B]): Monad[B] =
          from.flatMap(fn)

        def map[A, B](from: Monad[A], fn: A => B): Monad[B] =
          from.map(fn)
      }
    """
  }
}

trait Monadic[F[_]] {
  type Monad[T] = F[T]
  def point[A](value: A): F[A]
  def flatMap[A, B](from: F[A], fn: A => F[B]): F[B]
  def map[A, B](from: F[A], fn: A => B): F[B]
}
