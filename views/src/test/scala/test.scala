import scala.collection.par._
import scala.collection.views._
import scala.collection.views.ViewTransforms._
import scala.collection.par.Scheduler.Implicits.sequential

object A{
//val mymap = new Map[Int,Int](_ + 10)
  val myfilt = new Filter[Int](_ % 2 == 0)
//val mymap2 = new Map[Int,Double](_ * 1.0)
  val mystack = myfilt >> myfilt

  val xs = 0 until 11
  val v = View(xs.toPar) map mystack
  assert(v.size(x => true) == 6)
}
