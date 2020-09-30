package swc2.chapter11.ex11_4_4

import cats.kernel.CommutativeMonoid
import swc2.BoundedSemiLattice
import swc2.chapter11.ex11_3_3.BoundedSemilattice
import swc2.chapter11.ex11_3_3.BoundedSemilattice._

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}
object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) =
    counter
}

object GCounter {
  implicit def mapGCounter[K, V](): GCounter[Map[K, V], K, V] =
    new GCounter[Map[K, V], K, V] {
      override def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] =
        f + (k -> m.combine(f.getOrElse(k, m.empty), v))

      override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
        f2 ++ f1.map { case (k, v) => k -> b.combine(v, (f2.getOrElse(k, b.empty))) }

      override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.foldLeft(m.empty) { case (x, y) => m.combine(x, y) }
    }

}
