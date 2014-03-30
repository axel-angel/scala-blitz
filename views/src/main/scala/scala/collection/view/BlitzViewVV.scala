package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell


/** BlitzView implementation with a two sources Collections. */
abstract class BlitzViewVV[B] extends BlitzView[B] { self =>
  val xs: BlitzView[B] // 1st source view
  val ys: BlitzView[B] // 2nd source view

  override def >>[C](next: ViewTransform[B, C]) = new BlitzViewVV[C] {
    val xs = self.xs >> next
    val ys = self.ys >> next
  }

  override def map[C](next: ViewTransform[B, C]): BlitzView[C] = >> [C](next)
  override def map[C](f: B => C): BlitzViewVV[C] = self >> new Map[B,C](f)
  override def filter(p: B => Boolean): BlitzViewVV[B] = self >> new Filter[B](p)

  override def reduce(op: (B, B) => B)(implicit ctx: Scheduler): B = {
    val x = xs.reduce(op)(ctx)
    val y = ys.reduce(op)(ctx)
    op(x, y)
  }

  override def aggregate[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R = {
    val x = xs.aggregate(z)(op)(reducer)(ctx)
    val y = ys.aggregate(z)(op)(reducer)(ctx)
    reducer(x, y)
  }

  override def size()(implicit ctx: Scheduler): Int = xs.size() + ys.size()

  override def min()(implicit ord: Ordering[B], ctx: Scheduler): B =
    ord.min(xs.min(), ys.min())
  override def max()(implicit ord: Ordering[B], ctx: Scheduler): B =
    ord.max(xs.max(), ys.max())
}

