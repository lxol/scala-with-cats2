import cats.instances.int._
import cats.kernel.{CommutativeMonoid, Monoid}
import swc2.{BoundedSemiLattice, GCounter}

val g1 = Map("a" -> 7, "b" -> 3)
val g2 = Map("a" -> 2, "b" -> 5)

val counter = GCounter[Map, String, Int]