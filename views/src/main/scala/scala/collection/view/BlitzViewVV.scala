package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell


/** BlitzView implementation with a two sources Collections. */
abstract class BlitzViewVV[B] extends BlitzView[B] { self =>
  val xs: BlitzView[B] // 1st source view
  val ys: BlitzView[B] // 2nd source view

  override def >>[C](next: ViewTransform[B, C]) = new BlitzViewVV[C] {
    val xs = self.xs map next
    val ys = self.ys map next
  }

  override def map[C](next: ViewTransform[B, C]): BlitzView[C] = ???
  override def map[C](f: B => C): BlitzViewVV[C] = ???
  override def filter(p: B => Boolean): BlitzViewVV[B] = ???
  override def reduce(op: (B, B) => B)(implicit ctx: Scheduler): B = ???
  override def aggregate[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R = ???
  override def size()(implicit ctx: Scheduler): Int = ???
  override def min()(implicit ord: Ordering[B], ctx: Scheduler): B = ???
  override def max()(implicit ord: Ordering[B], ctx: Scheduler): B = ???
}

