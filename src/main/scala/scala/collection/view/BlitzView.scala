package scala.collection.views

import scala.collection.par._
import scala.collection.par.Scheduler.Implicits.sequential
import workstealing.ResultCell
import scala.collection.parallel.immutable.ParSeq


object BlitzView {

  abstract class ViewTransform[-A, +B] {
    self =>
    type Fold[A, F] = (A, ResultCell[F]) => ResultCell[F]

    def fold[F](g: Fold[B, F]): Fold[A, F]

    def >>[C](next: ViewTransform[B, C]) = new ViewTransform[A, C] {
      def fold[F](fd: Fold[C, F]): Fold[A, F] = self.fold(next.fold(fd))
    }
  }

  class Map[A, B](m: A => B) extends ViewTransform[A, B] {
    def fold[F](fd: Fold[B, F]): Fold[A, F] =
      (x, acc) => fd(m(x), acc)
  }

  class Filter[A](p: A => Boolean) extends ViewTransform[A, A] {
    def fold[F](fd: Fold[A, F]): Fold[A, F] =
      (x, acc) => if (p(x)) fd(x, acc) else acc
  }
}
