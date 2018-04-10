import estrapade.{TestApp, test}

import language.experimental.macros
import MagnoliaEncoder.genEncoder
import io.circe.Encoder
import io.circe.Encoder._

case class Recursive(field: Int, recursion: List[Recursive])

/**
  * Problem here is that we expect the same behaviour as shapeless Lazy provides -
  * the derived codec itself would be visible and used to construct the codec for List using circe std one.
  * Magnolia doesn't see it and derives coproduct codec for List instead, which doesn't look nice for json API:
  * {{{
  * {
  *   "::" : {
  *      "head" : {
  *        "field" : 2,
  *        "recursion" : {
  *           "Nil" : "Nil"
  *         }
  *       },
  *       "tl$access$1" : {
  *         "Nil" : "Nil"
  *       }
  *     }
  *   }
  * }
  * }}}
  */
object CirceRecursiveTypeTest extends TestApp {

  def tests(): Unit = {

    val encoder = Encoder[Recursive]
    test("Use available encoders while descending into a recursive type") {
      encoder(Recursive(1, List(Recursive(2, Nil), Recursive(3, Nil))))
    }
      .assert(
        j => j.asObject.flatMap(_ ("recursion")).exists(_.isArray),
        _.toString()
      )
  }
}
