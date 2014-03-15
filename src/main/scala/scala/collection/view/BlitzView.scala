package scala.collection.views

import scala.collection.par._
import scala.collection.par.Scheduler.Implicits.sequential
import workstealing.ResultCell
import scala.collection.parallel.immutable.ParSeq
import scala.collection.immutable.Range


object BlitzView {

  abstract class ViewTransform[-A, +B] {
    self =>
    type Fold[A, F] = (F, A) => F

    def fold[F](g: Fold[B, F]): Fold[A, F]

    def stack[C](next: ViewTransform[B, C]) = new ViewTransform[A, C] {
      def fold[F](fd: Fold[C, F]): Fold[A, F] = self.fold(next.fold(fd))
    }
  }

  class Map[A, B](m: A => B) extends ViewTransform[A, B] {
    def fold[F](fd: Fold[B, F]): Fold[A, F] =
      (acc, x) => fd(acc, m(x))
  }

  class Filter[A](p: A => Boolean) extends ViewTransform[A, A] {
    def fold[F](fd: Fold[A, F]): Fold[A, F] =
      (acc, x) => if (p(x)) fd(acc, x) else acc
  }
}
