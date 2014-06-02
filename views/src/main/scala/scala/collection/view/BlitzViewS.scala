package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell

/** BlitzView implementation with a single element. */
abstract class BlitzViewS[B] extends BlitzViewImpl[B] { self =>
  type A // type of source list
  val x: A // source element
  def transform: ViewTransform[A, B] // stack of transforms

  override def >>[C](next: ViewTransform[B, C]) = new BlitzViewS[C] {
    type A = self.A
    val x = self.x
    def transform = self.transform >> next
  }

  override def aggInternal[R](op: (B, ResultCell[R]) => ResultCell[R], pstop: ResultCell[R] => Boolean)(reducer: (R, R) => R)(implicit ctx: Scheduler): ResultCell[R] =
  {
    val rc = new ResultCell[R]
    transform.fold(op)(x, rc)
  }

}

