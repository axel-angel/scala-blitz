package scala.collection.views
import ViewTransforms._
import Scope._

import scala.collection.par._
import workstealing.ResultCell

/** BlitzView implementation with a two sources Collections. */
abstract class BlitzViewVV[B] extends BlitzViewImpl[B] { self =>
  val xs: BlitzView[B] // 1st source view
  val ys: BlitzView[B] // 2nd source view

  override def >>[C](next: ViewTransform[B, C]) = new BlitzViewVV[C] {
    val xs = self.xs >> next
    val ys = self.ys >> next
  }

  override def aggInternal[R](op: (B, ResultCell[R]) => ResultCell[R], pstop: ResultCell[R] => Boolean)(reducer: (R, R) => R)(implicit ctx: Scheduler): ResultCell[R] =
  {
    val rcx = xs.aggInternal(op, pstop)(reducer)(ctx)
    val rcy = ys.aggInternal(op, pstop)(reducer)(ctx)
    ViewUtils.rcCombine(reducer)(rcx, rcy)
  }
}

