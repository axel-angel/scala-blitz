package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell
import BlitzViewImpl._

/** BlitzView implementation with multiple flattened sources Collections. */
abstract class BlitzViewFlattenVs[U, B[_] <: BlitzView[_], A[_] <: BlitzView[_]] extends BlitzViewImpl[U] { self =>
  val zss: B[A[U]] // nested source view

  override def >>[C](next: ViewTransform[U, C]) = new BlitzViewFlattenVs[C, BlitzView, BlitzView] {
    val zss = self.zss.map{ x => x.asInstanceOf[BlitzView[U]] >> next }

  }

  override def genericInvoke[R](op: (U, ResultCell[R]) => ResultCell[R], pstop: ResultCell[R] => Boolean)(reducer: (R, R) => R)(implicit ctx: Scheduler): ResultCell[R] =
  {
    def folder(xss: A[U], rc: ResultCell[R]): ResultCell[R] = {
      val xrc = xss.genericInvoke(op, pstop)(reducer)(ctx)
      ViewUtils.rcCombine(reducer)(rc, xrc)
    }
    internalAPI(zss).genericInvoke(folder, pstop)(reducer)(ctx)
  }
}
