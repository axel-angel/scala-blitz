package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell


/** BlitzView implementation with a generating function as source. */
abstract class BlitzViewG[B] extends BlitzView[B] { self =>
  val g: Int => A // generating function as source

  /* TODO: this files contains only signature yet */
  def >>[C](next: ViewTransform[B, C]): BlitzViewG[C] = ???

  def map[C](f: B => C): BlitzViewG[C] = ???
  def filter(p: B => Boolean): BlitzViewG[B] = ???
  def reduce[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R = ???
  def size()(implicit ctx: Scheduler): Int = ???
  def min()(implicit ord: Ordering[B], ctx: Scheduler): Option[B] = ???
  def max()(implicit ord: Ordering[B], ctx: Scheduler): Option[B] = ???
}

