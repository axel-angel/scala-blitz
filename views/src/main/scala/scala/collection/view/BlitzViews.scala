package scala.collection.views

import scala.collection.par._
import scala.collection.par.Scheduler.Implicits.sequential
import workstealing.ResultCell
import scala.collection.parallel.ParSeq
import scala.collection.par.generic.IsReducable

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

    def reduce[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R): R = {
      def folder(x: B, cell: ResultCell[R]): ResultCell[R] = {
        cell.result = op(x, if (cell.isEmpty) z else cell.result)
        cell
      }
      xs.mapFilterReduce[R](transform.fold(folder))(reducer).result
    }

    def count(): Int = reduce(0)((_:B, x: Int) => x+1)(_ + _)

    def min()(implicit ord: Ordering[B]): Option[B] = {
      def foldMin(x: B, cur: ResultCell[B]): ResultCell[B] = {
        cur.result = if (cur.isEmpty || ord.gt(cur.result, x)) x else cur.result
        cur
      }
      def reduMin(x: B, y: B): B = if (ord.lt(x,y)) x else y
      xs.mapFilterReduce[B](transform.fold(foldMin))(reduMin).toOption
    }
    def max()(implicit ord: Ordering[B]): Option[B] = min()(ord.reverse)
  }

  object View {
    def apply[T, Repr](xss: Par[Repr])(implicit conv: IsReducable[Repr, T]): BlitzView[T] =
      apply(conv(xss))
    def apply[T](xss: Reducable[T]) = new BlitzView[T] {
      type A = T
      val xs = xss
      def transform = new Identity()
    }
  }
}
