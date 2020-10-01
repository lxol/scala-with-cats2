import swc2.Solution.GCounter
//import swc2.Solution.BoundedSemiLattice._
//import swc2.Solution._
import cats.instances.int
val counter1 = GCounter(Map[String, Int]())
counter1.increment("machine1", 5)
