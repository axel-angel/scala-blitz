package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell
import scala.collection.parallel.ParSeq
import scala.collection.par.generic.IsReducable

object BlitzViewsImpl {

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

  /** BlitzView implementation with a single source par Collection. */
  abstract class BlitzViewC[B] extends BlitzView[B] { self =>
    val xs: Reducable[A] // source list

    def >>[C](next: ViewTransform[B, C]) = new BlitzViewC[C] {
      type A = self.A
      val xs = self.xs
      def transform = self.transform >> next
    }

    def map[C](f: B => C): BlitzViewC[C] = self >> new Map[B,C](f)
    def filter(p: B => Boolean): BlitzViewC[B] = self >> new Filter[B](p)

    def reduce[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R = {
      def folder(x: B, cell: ResultCell[R]): ResultCell[R] = {
        cell.result = op(x, if (cell.isEmpty) z else cell.result)
        cell
      }
      xs.mapFilterReduce[R](transform.fold(folder))(reducer)(ctx).result
    }

    def count()(implicit ctx: Scheduler): Int = reduce(0)((_:B, x: Int) => x+1)(_ + _)(ctx)

    def min()(implicit ord: Ordering[B], ctx: Scheduler): Option[B] = {
      def foldMin(x: B, cur: ResultCell[B]): ResultCell[B] = {
        cur.result = if (cur.isEmpty || ord.gt(cur.result, x)) x else cur.result
        cur
      }
      def reduMin(x: B, y: B): B = if (ord.lt(x,y)) x else y
      xs.mapFilterReduce[B](transform.fold(foldMin))(reduMin)(ctx).toOption
    }
    def max()(implicit ord: Ordering[B], ctx: Scheduler): Option[B] = min()(ord.reverse, ctx)
  }

  object View {
    def apply[T, Repr](xss: Par[Repr])(implicit conv: IsReducable[Repr, T]): BlitzView[T] = new BlitzViewC[T] {
      type A = T
      val xs = conv(xss)
      def transform = new Identity()
    }
  }
}
