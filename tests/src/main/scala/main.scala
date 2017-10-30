package magnolia.tests

import magnolia._
import estrapade._
import contextual.data.scalac._
import contextual.data.fqt._
import contextual.data.txt._

import scala.util._

object Tests extends TestApp {

  def tests() = {

    test("construct a Show product instance") {
      import examples._
      Show.generic[Person].show(Person("John Smith", 34))
    }.assert(_ == """{name=John Smith,age=34}""")

    test("construct a Show coproduct instance") {
      import examples._
      Show.generic[Person].show(Person("John Smith", 34))
    }.assert(_ == "{name=John Smith,age=34}")
    
    test("serialize a Branch") {
      import magnolia.examples._
      implicitly[Show[Branch]].show(Branch(Leaf("LHS"), Leaf("RHS")))
    }.assert(_ == "{left={value=LHS},right={value=RHS}}")
   
    test("test equality false") {
      import examples._
      Eq.generic[Entity].equal(Person("John Smith", 34), Person("", 0))
    }.assert(_ == false)

    test("test equality true") {
      import examples._
      Eq.generic[Entity].equal(Person("John Smith", 34), Person("John Smith", 34))
    }.assert(_ == true)

    test("test branch equality true") {
      import examples._
      Eq.generic[Tree].equal(Branch(Leaf("one"), Leaf("two")), Branch(Leaf("one"), Leaf("two")))
    }.assert(_ == true)

    /*test("construction of Show instance for Leaf") {
      scalac"""
        import magnolia.examples._
        implicitly[Show[Leaf]]
      """
    }.assert(_ == Returns(fqt"magnolia.examples.Show[magnolia.examples.Leaf]"))
    
    test("construction of Show instance for Tree") {
      scalac"""
        import magnolia.examples._
        implicitly[Show[Tree]]
      """
    }.assert(_ == Returns(fqt"magnolia.examples.Show[magnolia.examples.Tree]"))
    
    test("serialize a Leaf") {
      import magnolia.examples._
      implicitly[Show[Leaf]].show(Leaf("testing"))
    }.assert(_ == "{value=testing}")
    
    test("serialize a Branch as a Tree") {
      import magnolia.examples._
      implicitly[Show[Tree]].show(Branch(Leaf("LHS"), Leaf("RHS")))
    }.assert(_ == "{left={value=LHS},right={value=RHS}}")

    test("show error stack") {
      scalac"""
        import magnolia.examples._
        case class Alpha(integer: Int)
        case class Beta(alpha: Alpha)
        Show.generic[Beta]
      """
    }.assert(_ == TypecheckError(txt"""magnolia: could not find typeclass for type Int
                                      |    in parameter 'integer' of product type Alpha
                                      |    in parameter 'alpha' of product type Beta
                                      |"""))*/
  }
}
