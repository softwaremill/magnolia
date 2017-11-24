package magnolia

package object examples {
  implicit class Extensions[T](t: T) {
    def show(implicit show: Show[String, T]): String = show.show(t)
    def ===[S >: T: Eq](that: S): Boolean = implicitly[Eq[S]].equal(t, that)
  }
}
