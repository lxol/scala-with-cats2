package swc2

import cats.kernel.Monoid
import cats.syntax.semigroup._
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.map._
import cats.instances.set._
import cats.kernel.CommutativeMonoid
trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}
object BoundedSemiLattice {
  implicit val inBoundedSemilattice: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
    override def combine(a1: Int, a2: Int): Int = a1.max(a2)

    override def empty: Int = 0
  }
  implicit def setBoundedSemilattice[A]: BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1.union(a2)

      override def empty: Set[A] = Set[A]()
    }
}
trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}
object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) =
    counter
  implicit def mapGCounter[K, V]: GCounter[Map, K, V] =
    new GCounter[Map, K, V] {
      override def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] =
        f |+| Map(k -> v)

      override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
        f1 |+| f2

      override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.toList.combineAll
    }
}
//final case class GCounter[A](counters: Map[String, A]) {
//  def increment(machine: String, amount: A)(implicit m: Monoid[A]): GCounter[A] =
//    GCounter(
//      counters |+| Map(machine ->  amount)
//      //counters + (machine -> (counters.get(machine).map(_ |+| amount).getOrElse(amount)))
//    )
//  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
//    GCounter(
//        counters |+| that.counters
//      //counters ++ that.counters.map { case (k, v) => (k, counters.get(k).map(b.combine(_, v)).getOrElse(v)) }
//    )
//  def total(implicit c: CommutativeMonoid[A]): A = counters.values.toList.combineAll
//}
//
