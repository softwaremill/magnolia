import scalaz._
import Scalaz._

@deriving(Show, Equal) sealed trait Alphabet

@deriving(Show, Equal) case class Greek(άλφα: Letter,
                                        βήτα: Letter,
                                        γάμα: Letter,
                                        δέλτα: Letter,
                                        έψιλον: Letter,
                                        ζήτα: Letter,
                                        ήτα: Letter,
                                        θήτα: Letter)
    extends Alphabet

@deriving(Show, Equal) case class Cyrillic(б: Letter,
                                           в: Letter,
                                           г: Letter,
                                           д: Letter,
                                           ж: Letter,
                                           з: Letter)
    extends Alphabet

@deriving(Show, Equal) case class Latin(a: Letter,
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

@deriving(Show, Equal) case class Letter(name: String, phonetic: String)
@deriving(Show, Equal) case class Country(name: String, language: Language)
@deriving(Show, Equal) case class Language(name: String, code: String, alphabet: Latin)

object Gen {
  Show[Alphabet]
  Equal[Alphabet]
}
