package swc2

object Solution {

  final case class GCounter(counters: Map[String, Int]) {

    final case class GCounter(counters: Map[String, Int]) {
      def increment(machine: String, amount: Int) =
        counters + (machine -> (counters.getOrElse(machine, 0) + amount))

      def merge(that: GCounter): GCounter =
        GCounter(
          counters ++ that.counters.map {
            case (k, v) => (k -> counters.get(k).map(_.max(v)).getOrElse(v))
          }
        )

      def total: Int = counters.values.sum
    }
  }
}
