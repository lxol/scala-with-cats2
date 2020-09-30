package swc2.chapter11.ex11_3_2


final case class GCounter(counters: Map[String, Int]) {

  /**
   * We are trying to spot the following laws:
   * Associativity : (A combine B) combine C  = A combine (B combine C)
   * Comutativity  : A combine B  = B + A
   * Identity :      A combine I = A
   * Idempotent :    A combine A = A
   */
  // '+' on Int has to be assoc, identity ///////////////////////////////////
  def increment(machine: String, amount: Int): GCounter =
    GCounter(counters + (machine -> (counters.getOrElse(machine, 0) + amount)))

  // max on Int should be assoc, commutative, identity and idempotent
  def merge(that: GCounter): GCounter =
    GCounter(
      that.counters ++ this.counters.map {
        case (k, v) => k -> (v.max(that.counters.getOrElse(k, 0)))
      }
    )
  //  '+' on Int associative, commutative, identity
  def total(): Int = counters.values.sum
}
//
