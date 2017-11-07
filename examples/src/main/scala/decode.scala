package magnolia.examples

import scala.language.existentials
import scala.language.higherKinds

import magnolia._
import scala.language.experimental.macros

trait Decoder[T] { def decode(str: String): T }

object Decoder {
  
  implicit val string: Decoder[String] = new Decoder[String] { def decode(str: String): String = str }
  implicit val int: Decoder[Int] = new Decoder[Int] { def decode(str: String): Int = str.toInt }
  implicit def generic[T]: Decoder[T] = macro Magnolia.generic[T]

  type Typeclass[T] = Decoder[T]
  
  def join[T](ctx: JoinContext[Decoder, T]): Decoder[T] = new Decoder[T] {
    def decode(value: String) = {
      val (name, values) = parse(value)
      ctx.construct { param => param.typeclass.decode(values(param.label)) }
    }
  }

  def dispatch[T](ctx: DispatchContext[Decoder, T]): Decoder[T] = new Decoder[T] {
    def decode(param: String) = {
      val (name, values) = parse(param)
      val subtype = ctx.subtypes.find(_.label == name).get
      subtype.typeclass.decode(param)
    }
  }

  private def parse(value: String): (String, Map[String, String]) = {
    val end = value.indexOf('(')
    val name = value.substring(0, end)
    
    def parts(value: String, idx: Int = 0, depth: Int = 0, collected: List[String] = List("")): List[String] = {
      def plus(char: Char): List[String] = collected.head+char :: collected.tail
      
      if(idx == value.length) collected
      else value(idx) match {
        case '(' =>
          parts(value, idx + 1, depth + 1, plus('('))
        case ')' =>
          if(depth == 1) plus(')')
          else parts(value, idx + 1, depth - 1, plus(')'))
        case ',' =>
          if(depth == 0) parts(value, idx + 1, depth, "" :: collected)
          else parts(value, idx + 1, depth, plus(','))
        case char =>
          parts(value, idx + 1, depth, plus(char))
      }
    }
    
    def keyValue(str: String): (String, String) = {
      val List(label, value) = str.split("=", 2).to[List]
      (label, value)
    }

    (name, parts(value.substring(end + 1, value.length - 1)).map(keyValue).toMap)
  }

}

