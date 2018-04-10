import estrapade.{TestApp, test}

import language.experimental.macros
import MagnoliaEncoder.genEncoder
import io.circe.Encoder


case class MapContainer(theMap: Map[String, List[Int]])

/**
  * Something weird is going on here.
  * During derivation for the right side of the map,
  * Magnolia notices default list codec only after it partially derives it as coproduct. Ending JSON is very bizarre:
  * {{{
  *  {
  *   "theMap" : {
  *     "f" : {
  *       "::" : [1 , 2, 3]
  *     }
  *   }
  * }
  * }}}
  * This can be fixed by explicitly importing Encoder companion into scope (see 2nd test).
  *
  * Seems like magnolia macro in some cases has higher priority than companion provided implicits,
  * that are not directly imported in scope
  */
object PriorityIssueTest extends TestApp {

  def tests(): Unit = {


    test("Use instances from companion even if they are not imported") {
      val encoder = Encoder[MapContainer]
      encoder(MapContainer(Map("f" -> List(1, 2, 3))))
    }
      .assert(
        j => j.hcursor.downField("theMap").downField("f").focus.exists(_.isArray),
        _.toString()
      )

    test("Use instances from companion when they are explicitly imported") {
      import Encoder._
      val encoder = Encoder[MapContainer]
      encoder(MapContainer(Map("f" -> List(1, 2, 3))))
    }
      .assert(
        j => j.hcursor.downField("theMap").downField("f").focus.exists(_.isArray),
        _.toString()
      )
  }
}
