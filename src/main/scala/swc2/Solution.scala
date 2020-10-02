package swc2

import cats.kernel.Monoid
import cats.syntax.semigroup._
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.map._

final case class GCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): GCounter  =
      GCounter(
          counters + (machine -> (counters.get(machine).map(_ + amount).getOrElse(amount)))
      )
  def merge(that: GCounter): GCounter =
      GCounter(
          counters ++ that.counters.map {case (k,v) => (k, counters.get(k).map(_ max v).getOrElse(v))}
      )
  def total: Int = counters.values.sum
}
