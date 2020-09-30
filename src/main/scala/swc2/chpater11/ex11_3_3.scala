package swc2.chapter11.ex11_3_3

import cats.kernel.{CommutativeMonoid, Monoid}

trait BoundedSemilattice[A] extends CommutativeMonoid[A] {
  def combine(x: A, y: A): A
  def empty: A
}

object BoundedSemilattice {
  implicit val intBoundedSemilattice = new BoundedSemilattice[Int] {
    def combine(x: Int, y: Int): Int = x.max(y)
    def empty: Int = 0
  }

  implicit def setBoundedSemilattice[A]() =
    new BoundedSemilattice[Set[A]] {
      def combine(s1: Set[A], s2: Set[A]): Set[A] = s1.union(s2)
      def empty: Set[A] = Set()
    }
}

final case class GCounter[A](counters: Map[String, A]) {

  /**
   * We are trying to spot the following laws:
   * Associativity : (A combine B) combine C  = A combine (B combine C)
   * Comutativity  : A combine B  = B + A
   * Identity :      A combine I = A
   * Idempotent :    A combine A = A
   */
  // '+' on Int has to be assoc, identity ///////////////////////////////////

  def increment(machine: String, amount: A)(implicit M: Monoid[A]): GCounter[A] =
    GCounter[A](counters + (machine -> M.combine(counters.getOrElse(machine, M.empty), amount)))

  // max on Int should be assoc, commutative, identity and idempotent
  def merge(that: GCounter[A])(implicit B: BoundedSemilattice[A]): GCounter[A] =
    GCounter[A](
      that.counters ++ this.counters.map {
        case (k, v) => k -> B.combine(v, (that.counters.getOrElse(k, B.empty)))
      }
    )
  //  '+' on Int associative, commutative, identity
  def total(implicit C: CommutativeMonoid[A]): A = counters.values.foldLeft(C.empty) { case (x, y) => C.combine(x, y) }
}
//
