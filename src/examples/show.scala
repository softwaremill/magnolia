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
package magnolia.examples

import magnolia._

/** shows one type as another, often as a string
  *
  *  Note that this is a more general form of `Show` than is usual, as it permits the return type to
  *  be something other than a string. */
trait Show[Out, T] { def show(value: T): Out }

trait GenericShow[Out] extends Derivation[[X] =>> Show[Out, X]] {

  def joinElems(typeName: String, strings: Seq[String]): Out
  def prefix(s: String, out: Out): Out

  /** creates a new [[Show]] instance by labelling and joining (with `mkString`) the result of
    *  showing each parameter, and prefixing it with the class name */
  def join[T](ctx: CaseClass[Typeclass, T]): Show[Out, T] = { value =>
    if ctx.isValueClass then
      val param = ctx.params.head
      param.typeclass.show(param.dereference(value))
    else
      val paramStrings = ctx.params.map { param =>
        val attribStr = if(param.annotations.isEmpty) "" else {
          param.annotations.mkString("{", ", ", "}")
        }
        val tpeAttribStr = if (param.typeAnnotations.isEmpty) "" else {
          param.typeAnnotations.mkString("{", ", ", "}")
        }
        s"${param.label}$attribStr$tpeAttribStr=${param.typeclass.show(param.dereference(value))}"
      }

      val anns = ctx.annotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
      val annotationStr = if (anns.isEmpty) "" else anns.mkString("{", ",", "}")

      val tpeAnns = ctx.typeAnnotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
      val typeAnnotationStr = if (tpeAnns.isEmpty) "" else tpeAnns.mkString("{", ",", "}")


      def typeArgsString(typeInfo: TypeInfo): String =
        if typeInfo.typeParams.isEmpty then ""
        else typeInfo.typeParams.map(arg => s"${ arg.short}${ typeArgsString(arg)}").mkString("[", ",", "]")

      joinElems(ctx.typeInfo.short + typeArgsString(ctx.typeInfo) + annotationStr + typeAnnotationStr, paramStrings)
  }

  /** choose which typeclass to use based on the subtype of the sealed trait
    * and prefix with the annotations as discovered on the subtype. */
  override def split[T](ctx: SealedTrait[Typeclass, T]): Show[Out, T] = (value: T) =>
    ctx.split(value) { sub =>
      val anns = sub.annotations.filterNot(_.isInstanceOf[scala.SerialVersionUID])
      val annotationStr = if (anns.isEmpty) "" else anns.mkString("{", ",", "}")

      prefix(annotationStr, sub.typeclass.show(sub.cast(value)))
    }
}

/** companion object to [[Show]] */
object Show extends GenericShow[String]:

  def prefix(s: String, out: String): String = s + out
  def joinElems(typeName: String, params: Seq[String]): String = params.mkString(s"$typeName(", ",", ")")

  given Show[String, String] = identity(_)
  given Show[String, Int] = _.toString
  given Show[String, Long] = _.toString + "L"
  given[A](using A: Show[String, A]): Show[String, Seq[A]] = _.iterator.map(A.show).mkString("[", ",", "]")