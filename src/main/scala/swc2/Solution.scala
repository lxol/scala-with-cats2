package swc2

import cats.kernel.Monoid
import cats.syntax.semigroup._
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.map._

object Solution {

  import cats.kernel.CommutativeMonoid
  trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }
  object BoundedSemiLattice {
    implicit val intBoundedSemilattice = new BoundedSemiLattice[Int] {
      override def combine(a1: Int, a2: Int): Int = a1.max(a2)
      override def empty: Int = 0
    }
    implicit def setBoundedSemilattice[A]() =
      new BoundedSemiLattice[Set[A]] {
        override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1.union(a2)

        override def empty: Set[A] = Set[A]()
      }
  }
  final case class GCounter[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit m: Monoid[A]): GCounter[A] =
      GCounter[A](
        counters |+| Map(machine -> amount)
      )

    def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
      GCounter[A](
        counters |+| that.counters
      )

    def total(implicit  m: CommutativeMonoid[A]): A = counters.values.toList.combineAll
  }
}
