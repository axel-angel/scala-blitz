package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell

/** BlitzView implementation with an optional element. */
abstract class BlitzViewO[B] extends BlitzViewImpl[B] { self =>
  type A // type of source list
  val ox: Option[A] // source element
  def transform: ViewTransform[A, B] // stack of transforms

  override def >>[C](next: ViewTransform[B, C]) = new BlitzViewO[C] {
    type A = self.A
    val ox = self.ox
    def transform = self.transform >> next
  }

  override def genericInvoke[R](op: (B, ResultCell[R]) => ResultCell[R], pstop: ResultCell[R] => Boolean)(reducer: (R, R) => R)(implicit ctx: Scheduler): ResultCell[R] =
  {
    val rc = new ResultCell[R]
    ox match {
      case Some(x) => transform.fold(op)(x, rc)
      case None => rc
    }
  }

}

