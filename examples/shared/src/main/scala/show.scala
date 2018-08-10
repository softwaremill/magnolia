/* Magnolia, version 0.10.0. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://co.ntextu.al/
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
package magnolia.examples

import magnolia._
import scala.language.experimental.macros

/** shows one type as another, often as a string
  *
  *  Note that this is a more general form of `Show` than is usual, as it permits the return type to
  *  be something other than a string. */
trait Show[Out, T] { def show(value: T): Out }

trait GenericShow[Out] {

  /** the type constructor for new [[Show]] instances
    *
    *  The first parameter is decided in the conrete implementation class,
    *  and the second parameter varies generically. */
  type Typeclass[T] = Show[Out, T]

  /** joins the values which were obtained by calling parameterValue for
    *  each of the parameters.
    */
  def join(paramValues: Seq[Out]): Out

  /** computes the value for the whole type after the individual parameters
    *  have been joined.
    */
  def typeValue(annotations: Seq[Any], typeName: String, joinedParameters: Out): Out

  /** computes the value of a single parameter in the case class
    */
  def parameterValue(annotations: Seq[Any], label: String, paramValue: Out): Out

  /** creates a new [[Show]] instance by labelling and joining the result of
    *  showing each parameter.
    *  Here is the generic part of the joining process and the actual implementation
    *  is in the implementation class. */
  def combine[T](ctx: CaseClass[Typeclass, T]): Show[Out, T] = { value =>
    if (ctx.isValueClass) {
      val param = ctx.parameters.head
      param.typeclass.show(param.dereference(value))
    } else {
      val paramValues: Seq[Out] = ctx.parameters.map { param =>
        parameterValue(param.annotations, param.label, param.typeclass.show(param.dereference(value)))
      }
      typeValue(ctx.annotations, ctx.typeName.short, join(paramValues))
    }
  }

  /** choose which typeclass to use based on the subtype of the sealed trait */
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Show[Out, T] = (value: T) =>
    ctx.dispatch(value) { sub =>
      sub.typeclass.show(sub.cast(value))
    }

  /** bind the Magnolia macro to this derivation object */
  implicit def gen[T]: Show[Out, T] = macro Magnolia.gen[T]
}

/** companion object to [[Show]] */
object Show extends GenericShow[String] {

  /** show typeclass for strings */
  implicit val string: Show[String, String] = (s: String) => s

  def join(paramValues: Seq[String]): String = paramValues.mkString(",")

  def typeValue(annotations: Seq[Any], typeName: String, joinedParameters: String): String = {
    val anns = annotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
    val annotationStr = if (anns.isEmpty) "" else anns.mkString("[", ",", "]")

    s"$annotationStr$typeName($joinedParameters)"
  }

  def parameterValue(annotations: Seq[Any], label: String, paramValue: String): String = {
    val attribStr = if(annotations.isEmpty) "" else {
      annotations.mkString("{", ", ", "}")
    }
    s"${label}$attribStr=${paramValue}"
  }

  /** show typeclass for integers */
  implicit val int: Show[String, Int] = (s: Int) => s.toString

  /** show typeclass for sequences */
  implicit def seq[A](implicit A: Show[String, A]): Show[String, Seq[A]] =
    new Show[String, Seq[A]] {
      def show(as: Seq[A]): String = as.iterator.map(A.show).mkString("[", ",", "]")
    }
}


/** Another implementation for GenericShow using Int.
  *  The result from this transformation is the same
  *  as which is produced by the WeakHash in hash.scala. */
object ShowWeakHash extends GenericShow[Int] {

  /** show typeclass for strings */
  implicit val string: Show[Int, String] = (s: String) => s.map(_.toInt).sum

  def join(paramValues: Seq[Int]): Int = paramValues.foldLeft(0)(_ ^ _)

  def typeValue(annotations: Seq[Any], typeName: String, joinedParameters: Int): Int = {
    joinedParameters
  }

  def parameterValue(annotations: Seq[Any], label: String, paramValue: Int): Int = {
    paramValue
  }

  /** show typeclass for integers */
  implicit val int: Show[Int, Int] = (i: Int) => i

  /** show typeclass for sequences */
  implicit def seq[A](implicit A: Show[Int, A]): Show[Int, Seq[A]] =
    new Show[Int, Seq[A]] {
      def show(as: Seq[A]): Int = as.iterator.map(A.show).sum
    }
}
