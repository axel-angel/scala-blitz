import scala.collection.par._
import scala.collection.views.BlitzViews._
import scala.collection.par.Scheduler.Implicits.sequential

val mymap = new Map[Int,Int](_ + 10)
val myfilt = new Filter[Int](_ % 2 == 0)
val mymap2 = new Map[Int,Double](_ * 1.0)
val mystack = mymap >> myfilt >> mymap2

val xs = 0 until 11
val v = View(par2zippable(xs.toPar)) >> mystack
v.count()
