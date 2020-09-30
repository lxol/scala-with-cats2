package swc2.chpater11

import cats.kernel.CommutativeMonoid

trait BoundedSemilattice[A] extends CommutativeMonoid[A] {

  def combine(x: A, y: A): A

  def empty: A
}
