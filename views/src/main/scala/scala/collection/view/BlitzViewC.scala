package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell
import java.util.NoSuchElementException

/** BlitzView implementation with a single source par Collection. */
abstract class BlitzViewC[B] extends BlitzViewImpl[B] { self =>
  type A // type of source list
  val xs: Reducable[A] // source list
  def transform: ViewTransform[A, B] // stack of transforms

  def >>[C](next: ViewTransform[B, C]) = new BlitzViewC[C] {
    type A = self.A
    val xs = self.xs
    def transform = self.transform >> next
  }

  override def aggInternal[R](op: (B, ResultCell[R]) => ResultCell[R], pstop: ResultCell[R] => Boolean)(reducer: (R, R) => R)(implicit ctx: Scheduler): ResultCell[R] =
  {
    val stopper = ViewUtils.toStopper(pstop)_
    xs.mapFilterReduce[R](transform.fold(op), stopper)(reducer)(ctx)
  }

}

