package scala.collection.views
import ViewTransforms._

import scala.collection.par._
import workstealing.ResultCell
import java.util.NoSuchElementException

/** BlitzView implementation with a single source par Collection. */
abstract class BlitzViewC[B] extends BlitzView[B] { self =>
  type A // type of source list
  val xs: Reducable[A] // source list
  def transform: ViewTransform[A, B] // stack of transforms

  override def >>[C](next: ViewTransform[B, C]) = new BlitzViewC[C] {
    type A = self.A
    val xs = self.xs
    def transform = self.transform >> next
  }

  override def map[C](next: ViewTransform[B, C]): BlitzView[C] = >> [C](next)
  override def map[C](f: B => C): BlitzViewC[C] = self >> new Map[B,C](f)
  override def filter(p: B => Boolean): BlitzViewC[B] = self >> new Filter[B](p)

  override def reduceOpt(op: (B, B) => B)(implicit ctx: Scheduler): Option[B] = {
    def folder(x: B, cell: ResultCell[B]): ResultCell[B] = {
      cell.result = if (cell.isEmpty) x else op(x, cell.result)
      cell
    }
    val r = xs.mapFilterReduce[B](transform.fold(folder))(op)(ctx)
    r.toOption
  }

  override def aggregate[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R = {
    def folder(x: B, cell: ResultCell[R]): ResultCell[R] = {
      cell.result = op(x, if (cell.isEmpty) z else cell.result)
      cell
    }
    xs.mapFilterReduce[R](transform.fold(folder))(reducer)(ctx).result
  }

  override def size()(implicit ctx: Scheduler): Int =
    aggregate(0)((_:B, x: Int) => x+1)(_ + _)(ctx)

  override def count(p: B => Boolean)(implicit ctx: Scheduler): Int =
    aggregate(0)((x: B, c: Int) => c  + (if (p(x)) 1 else 0))(_ + _)

  override def minOpt()(implicit ord: Ordering[B], ctx: Scheduler): Option[B] = {
    def foldMin(x: B, cur: ResultCell[B]): ResultCell[B] = {
      cur.result = if (cur.isEmpty || ord.gt(cur.result, x)) x else cur.result
      cur
    }
    def reduMin(x: B, y: B): B = if (ord.lt(x,y)) x else y
    val r = xs.mapFilterReduce[B](transform.fold(foldMin))(reduMin)(ctx)
    r.toOption
  }
  override def maxOpt()(implicit ord: Ordering[B], ctx: Scheduler): Option[B] =
    minOpt()(ord.reverse, ctx)

  override def find(p: B => Boolean)(implicit ctx: Scheduler): Option[B] = {
    def folder(x: B, o: Option[B]): Option[B] =
      if (p(x)) o.orElse(Some(x)) else o
    aggregate(None: Option[B])(folder)(_.orElse(_))
  }

  override def exists(p: B => Boolean)(implicit ctx: Scheduler): Boolean =
    aggregate(false)(p(_) || _)(_ || _)

  override def forall(p: B => Boolean)(implicit ctx: Scheduler): Boolean =
    aggregate(true)(p(_) && _)(_ && _)
}

