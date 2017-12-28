import adt._
import cats._
import derived._
import cats.Show
import cats.instances.all._

sealed trait Alphabet

case class Greek(άλφα: Letter,
                 βήτα: Letter,
                 γάμα: Letter,
                 δέλτα: Letter,
                 έψιλον: Letter,
                 ζήτα: Letter,
                 ήτα: Letter,
                 θήτα: Letter)
    extends Alphabet

case class Cyrillic(б: Letter, в: Letter, г: Letter, д: Letter, ж: Letter, з: Letter)
    extends Alphabet

case class Latin(a: Letter,
                 b: Letter,
                 c: Letter,
                 d: Letter,
                 e: Letter,
                 f: Letter,
                 g: Letter,
                 h: Letter,
                 i: Letter,
                 j: Letter,
                 k: Letter,
                 l: Letter,
                 m: Letter,
                 n: Letter,
                 o: Letter,
                 p: Letter,
                 q: Letter,
                 r: Letter,
                 s: Letter,
                 t: Letter,
                 u: Letter,
                 v: Letter)
    extends Alphabet

case class Letter(name: String, phonetic: String)
case class Country(name: String, language: Language)
case class Language(name: String, code: String, alphabet: Latin)

object Gen {
  derive.show[Alphabet]
  derive.eq[Alphabet]
}
