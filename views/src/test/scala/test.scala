import scala.collection.par._
import scala.collection.views._
import scala.collection.views.ViewTransforms._
import scala.collection.par.Scheduler.Implicits.sequential

object A {
  def main(args: Array[String]) {
    val xs = (0 to 10).toList
    test1(xs, View((0 to 10).toPar))
    test1(xs, View((0 until 11).toPar))
    test1(xs, View(View((0 to 5).toPar), View((6 to 10).toPar)))
    test1(xs, recView(0 to 10))
    test1(xs, View(
      View(
        View(vrange(0,1),vrange(2,3)),
        View(vrange(4,5),vrange(6,6))
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

  // Compare BlitzView and List transformations, should stay consistent
  def test1(l1: List[Int], v1: BlitzView[Int]) {
    val v = v1 map{_ + 10} filter{_ % 2 == 0} map{_ * 1.0}
    val l = l1 map{_ + 10} filter{_ % 2 == 0} map{_ * 1.0}
    testFolders(l, v)
  }

  // Compare the results with different folding methods
  def testFolders(l: List[Double], v: BlitzView[Double]) {
    assert(v.toList == l)
    assert(v.toArray.toList == l)
    assert(v.size == l.size)

    assert(v.count{_ => true} == l.count{_ => true})
    assert(v.count{_ % 4 == 0} == l.count{_ % 4 == 0})

    assert(v.aggregate(0.0){_ + _}{_ + _} == l.aggregate(0.0)(_ + _, _ + _))
    assert(v.reduce{_ + _} == l.reduce{_ + _})
    assert(v.min == l.min)
    assert(v.max == l.max)

    assert(v.find{_ == 20.0} == l.find{_ == 20.0})
    assert(v.find{_ == 21.0} == l.find{_ == 21.0})

    assert(v.exists{_ >  20.0} == l.exists{_ >  20.0})
    assert(v.exists{_ >= 20.0} == l.exists{_ >= 20.0})

    assert(v.forall{_ != 0.0} == l.forall{_ != 0.0})
    assert(v.forall{x => x >= 10 && x <= 20}
        == l.forall{x => x >= 10 && x <= 20})

    assert(v.sum == l.sum)
    assert(v.product == l.product)

    println("tests passed")
  }
}
