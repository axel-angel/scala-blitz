import scala.collection.par._
import scala.collection.views._
import scala.collection.views.ViewTransforms._
import scala.collection.par.Scheduler.Implicits.sequential

object A {
  def main(args: Array[String]) {
    testCount(View((0 to 10).toPar))
    testCount(View((0 until 11).toPar))
    testCount(View(View((0 to 5).toPar), View((5 to 10).toPar)))
  }

  def testCount(v1: BlitzView[Int]) {
    val v = v1 map{_ + 10} filter{_ % 2 == 0} map{_ * 1.0}
    assert(v.size() == 6)

    assert(v.aggregate(0.0){_ + _}{_ + _} == 90.0)
    assert(v.reduce{_ + _} == 90.0)
    assert(v.min == 10.0)
    assert(v.max == 20.0)

    View((0 until 0).toPar).reduce(_ + _) // FIXME: this should fail!

    println("tests passed")
  }
}
