import estrapade.{TestApp, test}

import language.experimental.macros
import MagnoliaEncoder.genEncoder
import MagnoliaDecoder.genDecoder
import io.circe._

/**
  * This is another semantical difference from shapeless-based circe derivation.
  * Shapeless generic skips intermediate traits/abstract classes and forms a coproduct of only leaf types.
  * This makes perfect sense for many scenarios, JSON included.
  *
  * Magnolia, on the other hand, dispatches through all intermediate types.
  * For encoding it's not that bad, but for decoding it's a showstopper. See tests.
  */
object IntermediateTraitsTest extends TestApp {

  sealed trait T
  case class A(a: Int) extends T
  case class B(b: String) extends T
  sealed trait C extends T
  case class C1(c1: Int) extends C
  case class C2(c2: String) extends C

  def tests(): Unit = {

    val encoder = Encoder[T]
    val decoder = Decoder[T]

    // here JSON is deeper nested than when using circe-generic.
    // it's not that huge problem, until you try to decode a leaf, that is under an intermediate trait (next test)
    test("Skip intermediate traits on encoding") {
      encoder(C1(5)).hcursor.get[JsonObject]("C1")
    }
      .assert(
        _.isRight,
        _.toString()
      )

    // when sending a message to JSON API we don't usually specify intermediate traits - we just put the leaf type into the key.
    // Magnolia can't see the C1, because on the first dispatch it faces only A, B and C.
    test("Skip intermediate traits on decoding") {
      decoder(HCursor.fromJson(Json.obj("C1" -> Json.obj("c1" -> Json.fromInt(2)))))
    }
      .assert(
        _.isRight,
        _.toString()
      )

  }
}
