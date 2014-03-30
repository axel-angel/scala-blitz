import scala.collection.par._
import scala.collection.views._
import scala.collection.views.ViewTransforms._
import scala.collection.par.Scheduler.Implicits.sequential

object A {
  def main(args: Array[String]) {
    testCount()
  }

  def testCount() {
    val xs = 0 until 11
    val v = View(xs.toPar) map{_ + 10} filter{_ % 2 == 0} map{_ * 1.0}
    assert(v.size() == 6)

    val sum = v aggregate(0.0){_ + _}{_ + _}
    assert(sum == 90.0)
  }
}
