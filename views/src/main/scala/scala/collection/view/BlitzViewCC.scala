package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell


/** BlitzView implementation with a two sources Collections. */
abstract class BlitzViewCC[B] extends BlitzView[B] { self =>
  val xs: Reducable[A] // 1st source list
  val ys: Reducable[A] // 2nd source list

  /* TODO: this files contains only signature yet */
  def >>[C](next: ViewTransform[B, C]): BlitzViewCC[C] = ???

  def map[C](f: B => C): BlitzViewCC[C] = ???
  def filter(p: B => Boolean): BlitzViewCC[B] = ???
  def reduce[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R = ???
  def size()(implicit ctx: Scheduler): Int = ???
  def min()(implicit ord: Ordering[B], ctx: Scheduler): Option[B] = ???
  def max()(implicit ord: Ordering[B], ctx: Scheduler): Option[B] = ???
}

