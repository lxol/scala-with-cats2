package swc2
import cats.kernel.CommutativeMonoid
import cats.implicits._
import cats.kernel.Monoid

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {

  implicit val intBoundedsemilattice = new BoundedSemiLattice[Int] {
    def combine(a1: Int, a2: Int): Int = a1 max a2
    def empty: Int = 0
  }

  implicit def setsBoundedSemiLattice[A](): BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      def combine(s1: Set[A], s2: Set[A]): Set[A] = s1 combine s2
      def empty: Set[A] = Set[A]()
    }
}

final case class GCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(
      implicit M: Monoid[A]
  ): GCounter[A] =
    GCounter(
      counters + (machine -> M
        .combine(amount, counters.getOrElse(machine, M.empty)))
    )

  def merge(
      that: GCounter[A]
  )(implicit B: BoundedSemiLattice[A]): GCounter[A] = {
    GCounter(this.counters |+| that.counters)
  }

  def totalm(implicit C: CommutativeMonoid[A]): A =
    // counters.values.foldLeft(C.empty)((acc, e) => C.combine(acc, e))
    counters.values.toList.combineAll
}
