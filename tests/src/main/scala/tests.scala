package magnolia.tests

import magnolia._
import estrapade._
import contextual.data.scalac._
import contextual.data.fqt._
import contextual.data.txt._

import scala.util._

object Tests extends TestApp {

  def tests() = {
    import examples._

    test("construct a Show product instance") {
      import examples._
      Show.generic[Person].show(Person("John Smith", 34))
    }.assert(_ == """Person(name=John Smith,age=34)""")

    test("construct a Show coproduct instance") {
      import examples._
      Show.generic[Person].show(Person("John Smith", 34))
    }.assert(_ == "Person(name=John Smith,age=34)")
    
    Show.generic[Tree]

    test("serialize a Branch") {
      import magnolia.examples._
      implicitly[Show[String, Branch]].show(Branch(Leaf("LHS"), Leaf("RHS")))
    }.assert(_ == "Branch(left=Leaf(value=LHS),right=Leaf(value=RHS))")

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

    test("construct a default value") {
      Default.generic[Entity].default
    }.assert(_ == (Company(""): Entity))

    test("construction of Show instance for Leaf") {
      scalac"""
        import magnolia.examples._
        implicitly[Show[String, Leaf]]
      """
    }.assert(_ == (Returns(fqt"magnolia.examples.Show[String,magnolia.examples.Leaf]"): Compilation))
    
    test("construction of Show instance for Tree") {
      scalac"""
        import magnolia.examples._
        implicitly[Show[String, Tree]]
      """
    }.assert(_ == (Returns(fqt"magnolia.examples.Show[String,magnolia.examples.Tree]"): Compilation))
    
    test("serialize a Leaf") {
      implicitly[Show[String, Leaf]].show(Leaf("testing"))
    }.assert(_ == "Leaf(value=testing)")
    
    test("serialize a Branch as a Tree") {
      implicitly[Show[String, Tree]].show(Branch(Leaf("LHS"), Leaf("RHS")))
    }.assert(_ == "Branch(left=Leaf(value=LHS),right=Leaf(value=RHS))")

    test("show error stack") {
      scalac"""
        import magnolia.examples._
        case class Alpha(integer: Double)
        case class Beta(alpha: Alpha)
        Show.generic[Beta]
      """
    }.assert(_ == (TypecheckError(txt"""magnolia: could not find typeclass for type Double
                                      |    in parameter 'integer' of product type Alpha
                                      |    in parameter 'alpha' of product type Beta
                                      |"""): Compilation))
    
    //test("construct a decoder") {
    //Decoder.generic[Tree].decode("string")
    //}.assert(_ == (Leaf("something"): Tree))

  }
}
