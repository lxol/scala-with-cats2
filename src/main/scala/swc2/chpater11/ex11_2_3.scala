package swc2.chapter11.ex11_2_3

final case class GCounter(counters: Map[String, Int]) {

  def increment(machine: String, amount: Int) = ???

  def merge(that: GCounter) = ???

  def totalm: Int = ???

}
