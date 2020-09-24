package swc2

final case class GCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): GCounter = {
    val value = amount + counters.getOrElse(machine, 0)
    GCounter(counters + (machine -> value))
  }

  def merge(that: GCounter): GCounter = {
    GCounter(counters ++ that.counters.map {
      case (k, v) => (k, v max that.counters.getOrElse(k, 0))
    })

  }
  def total: Int = counters.values.sum

}
