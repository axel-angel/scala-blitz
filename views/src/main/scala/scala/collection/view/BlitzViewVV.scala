package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell


/** BlitzView implementation with a two sources Collections. */
abstract class BlitzViewVV[B] extends BlitzView[B] { self =>
  val xs: BlitzView[A] // 1st source view
  val ys: BlitzView[A] // 2nd source view

  /* TODO: this files contains only signature yet */
  def >>[C](next: ViewTransform[B, C]): BlitzViewVV[C] = ???

  override def map[C](next: ViewTransform[B, C]): BlitzView[C] = ???
  override def map[C](f: B => C): BlitzViewVV[C] = ???
  override def filter(p: B => Boolean): BlitzViewVV[B] = ???
  override def aggregate[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R = ???
  override def size()(implicit ctx: Scheduler): Int = ???
  override def min()(implicit ord: Ordering[B], ctx: Scheduler): B = ???
  override def max()(implicit ord: Ordering[B], ctx: Scheduler): B = ???
}

