package scala.collection.views
import ViewTransforms._
import ViewUtils._

import scala.collection.par._
import workstealing.ResultCell


/** BlitzView implementation with a two sources Collections. */
abstract class BlitzViewVV[B] extends BlitzViewImpl[B] { self =>
  val xs: BlitzViewImpl[B] // 1st source view
  val ys: BlitzViewImpl[B] // 2nd source view

  def >>[C](next: ViewTransform[B, C]) = new BlitzViewVV[C] {
    val xs = self.xs >> next
    val ys = self.ys >> next
  }

  override def map[C](f: B => C): BlitzViewVV[C] = self >> new Map[B,C](f)
  override def filter(p: B => Boolean): BlitzViewVV[B] = self >> new Filter[B](p)

  override def reduceOpt(op: (B, B) => B)(implicit ctx: Scheduler): Option[B] = {
    val x = xs.reduceOpt(op)(ctx)
    val y = ys.reduceOpt(op)(ctx)
    ViewUtils.optCombine(op)(x, y)
  }

  override def aggregate[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R = {
    val x = xs.aggregate(z)(op)(reducer)(ctx)
    val y = ys.aggregate(z)(op)(reducer)(ctx)
    reducer(x, y)
  }

  override def size()(implicit ctx: Scheduler): Int = xs.size() + ys.size()

  override def count(p: B => Boolean)(implicit ctx: Scheduler): Int =
    xs.count(p) + ys.count(p)

  override def minOpt()(implicit ord: Ordering[B], ctx: Scheduler): Option[B] =
    optCombine(ord.min)(xs.minOpt(), ys.minOpt())
  override def maxOpt()(implicit ord: Ordering[B], ctx: Scheduler): Option[B] =
    optCombine(ord.max)(xs.maxOpt(), ys.maxOpt())

  def find(p: B => Boolean)(implicit ctx: Scheduler): Option[B] =
    xs.find(p) orElse ys.find(p)

  def exists(p: B => Boolean)(implicit ctx: Scheduler): Boolean =
    xs.exists(p) || ys.exists(p)

  def forall(p: B => Boolean)(implicit ctx: Scheduler): Boolean =
    xs.forall(p) && ys.forall(p)
}

