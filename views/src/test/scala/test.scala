import scala.collection.par._
import scala.collection.views._
import scala.collection.views.ViewTransforms._
import scala.collection.par.Scheduler.Implicits.sequential

object A {
  def main(args: Array[String]) {
    testCount(View((0 to 10).toPar))
    testCount(View((0 until 11).toPar))
    testCount(View(View((0 to 5).toPar), View((5 to 10).toPar)))
    testCount(recView(0 to 10))
    testCount(View(
      View(
        View(vrange(0,1),vrange(2,3)),
        View(vrange(4,5),vrange(5,6))
      ),
      View(
        vrange(7,7),
        vrange(8,10)
      )
    ))
  }

  def vrange(x: Int, y: Int) = View((x to y).toPar)

  def recView(xs: Range): BlitzView[Int] = {
    if (xs.length > 1) {
      val (l, r) = xs.splitAt(xs.length/2)
      View(recView(l), recView(r))
    } else {
      View(xs.toPar)
    }
  }

  def testCount(v1: BlitzView[Int]) {
    val v = v1 map{_ + 10} filter{_ % 2 == 0} map{_ * 1.0}
    assert(v.size() == 6)

    assert(v.count{_ => true} == 6)
    assert(v.count{_ % 4 == 0} == 3)

    assert(v.aggregate(0.0){_ + _}{_ + _} == 90.0)
    assert(v.reduce{_ + _} == 90.0)
    assert(v.min == 10.0)
    assert(v.max == 20.0)

    assert(v.find{_ == 20.0} == Some(20.0))
    assert(v.find{_ == 21.0} == None)

    assert(v.exists{_ >  20.0} == false)
    assert(v.exists{_ >= 20.0} == true)

    assert(v.forall{_ != 0.0} == true)
    assert(v.forall{x => x >= 10 && x <= 20} == true)

    println("tests passed")
  }
}
