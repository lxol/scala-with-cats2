package swc2.chapter11.ex11_2_3

final case class GCounter(counters: Map[String, Int]) {

  /**
   * We are trying to spot the following laws:
   * Associativity : (A combine B) combine C  = A combine (B combine C)
   * Comutativity  : A combine B  = B + A
   * Identity :      A combine I = A
   * Idempotent :    A combine A = A
   */
  // assoc, identity ///////////////////////////////////////////
  def increment(machine: String, amount: Int): GCounter =
    GCounter(counters + (machine -> (counters.getOrElse(machine, 0) + amount)))

  // max should be assoc, communicate, identity and idempotent
  def merge(that: GCounter): GCounter =
    GCounter(
      that.counters ++ this.counters.map {
        case (k, v) => k -> (v.max(that.counters.getOrElse(k, 0)))
      }
    )
  //  assoc, comm, identity 
  def total(): Int = counters.values.sum

}

//
