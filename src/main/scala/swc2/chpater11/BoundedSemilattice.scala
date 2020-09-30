package swc2.chpater11

import cats.kernel.CommutativeMonoid

trait BoundedSemilattice[A] extends CommutativeMonoid[A] {

  def combine(x: A, y: A): A

  def empty: A
}

object BoundedSemilattice {
  implicit val intBoundedSemilattice = new BoundedSemilattice[Int] {
    def combine(x: Int, y: Int): Int = x.max(y)
    def empty: Int = 0
  }

  implicit def setBoundedSemilattice[A]() = new BoundedSemilattice[Set[A]] {
    def combine(s1: Set[A], s2: Set[A]): Set[A] = s1 union s2
    def empty: Set[A] = Set()
  }
}
