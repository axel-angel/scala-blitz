package scala.collection.views

import scala.collection.Parallelizable
import scala.collection.par._
import scala.collection.par.Scheduler.Implicits.sequential
import workstealing.ResultCell

object BlitzViews {

  abstract class ViewTransform[-A, +B] {
    self =>
    type Fold[A, F] = (A, ResultCell[F]) => ResultCell[F]

    def fold[F](g: Fold[B, F]): Fold[A, F]

    def >>[C](next: ViewTransform[B, C]) = new ViewTransform[A, C] {
      def fold[F](fd: Fold[C, F]): Fold[A, F] = self.fold(next.fold(fd))
    }
  }

  class Identity[A] extends ViewTransform[A, A] {
    def fold[F](fd: Fold[A, F]): Fold[A, F] = fd
  }

  class Map[A, B](m: A => B) extends ViewTransform[A, B] {
    def fold[F](fd: Fold[B, F]): Fold[A, F] =
      (x, acc) => fd(m(x), acc)
  }

  class Filter[A](p: A => Boolean) extends ViewTransform[A, A] {
    def fold[F](fd: Fold[A, F]): Fold[A, F] =
      (x, acc) => if (p(x)) fd(x, acc) else acc
  }

  abstract class BlitzView[B] {
    self =>

    val xs: Reducable[A] // source list
    type A // type of source list

    def transform: ViewTransform[A, B] // stack of transforms

    def >>[C](next: ViewTransform[B, C]) = new BlitzView[C] {
      type A = self.A
      val xs = self.xs
      def transform = self.transform >> next
    }

    def map[C](f: B => C): BlitzView[C] = self >> new Map[B,C](f)
    def filter(p: B => Boolean): BlitzView[B] = self >> new Filter[B](p)

    def reduce[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R) = {
      def folder(x: B, cell: ResultCell[R]): ResultCell[R] = {
        cell.result = op(x, if (cell.isEmpty) z else cell.result)
        cell
      }
      // FIXME: need to split into own module due to macro stage restriction
      ???//xs.mapFilterReduce[R](transform.fold(folder))(reducer)
    }

    //def force = xs.map(f)
  }

  object View {
    //def apply[T](xss: Parallelizable[T, Reducable[T]]): BlitzView[T] = apply(xss.par)
    def apply[T](xss: Reducable[T]) = new BlitzView[T] {
      type A = T
      val xs = xss
      def transform = new Identity()
    }
  }
}
