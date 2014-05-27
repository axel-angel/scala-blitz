package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell

/** BlitzView implementation with multiple flattened sources Collections. */
abstract class BlitzViewFlattenVs[B] extends BlitzViewImpl[B] { self =>
  val zss: BlitzViewImpl[BlitzViewImpl[B]] // nested source view

  override def >>[C](next: ViewTransform[B, C]) = new BlitzViewFlattenVs[C] {
    val zss = self.zss.map{ xs: BlitzViewImpl[B] => xs >> next }
  }

  override def aggInternal[R](op: (B, ResultCell[R]) => ResultCell[R], pstop: ResultCell[R] => Boolean)(reducer: (R, R) => R)(implicit ctx: Scheduler): ResultCell[R] =
  {
    def folder(xss: BlitzViewImpl[B], rc: ResultCell[R]): ResultCell[R] = {
      val xrc = xss.aggInternal(op, pstop)(reducer)(ctx)
      ViewUtils.rcCombine(reducer)(xrc, rc)
    }
    zss.aggInternal(folder, pstop)(reducer)(ctx)
  }
}
