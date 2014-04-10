package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell

trait ViewTransform[-A, +B] {
  self =>
  type Fold[A, F] = (A, ResultCell[F]) => ResultCell[F]

  def fold[F](g: Fold[B, F]): Fold[A, F]

  def >>[C](next: ViewTransform[B, C]) = new ViewTransform[A, C] {
    def fold[F](fd: Fold[C, F]): Fold[A, F] = self.fold(next.fold(fd))
  }
}

trait BlitzView[B] { self =>
  def >>[C](next: ViewTransform[B, C]): BlitzView[C] // TODO: shouldn't be here

  /* methods: V -> V */
  def map[C](next: ViewTransform[B, C]): BlitzView[C]
  def map[C](f: B => C): BlitzView[C]
  def filter(p: B => Boolean): BlitzView[B]
  def drop(n: Int): BlitzView[B] = ???
  def take(n: Int): BlitzView[B] = ???
  //def flatten[B <: BlitzView[C]](): BlitzView[C] = ??? // TODO

  /* methods: V -> other array structure */
  def toArray(): Array[B] = ???

  /* methods: V -> V[constant type] */
  def toInts(): BlitzView[Int] = ???
  def toDoubles(): BlitzView[Double] = ???
  def toFloats(): BlitzView[Float] = ???
  def toBooleans(): BlitzView[Boolean] = ???

  /* methods: V -> 1 */
  def reduceOpt(op: (B, B) => B)(implicit ctx: Scheduler): Option[B]
  def reduce(op: (B, B) => B)(implicit ctx: Scheduler): B =
    reduceOpt(op)(ctx).get // throws an Exception if empty
  def aggregate[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R
  def minOpt()(implicit ord: Ordering[B], ctx: Scheduler): Option[B]
  def maxOpt()(implicit ord: Ordering[B], ctx: Scheduler): Option[B]
  def min()(implicit ord: Ordering[B], ctx: Scheduler): B =
    minOpt()(ord, ctx).get // throws an Exception if empty
  def max()(implicit ord: Ordering[B], ctx: Scheduler): B =
    maxOpt()(ord, ctx).get // throws an Exception if empty
  def sum()(implicit num: Numeric[B], ctx: Scheduler): B =
    aggregate(num.zero)(num.plus(_, _))(num.plus(_, _))

  /* methods: V -> 1[constant type] */
  def size()(implicit ctx: Scheduler): Int
  def count(p: B => Boolean)(implicit ctx: Scheduler): Int
  def find(p: B => Boolean)(implicit ctx: Scheduler): Option[B]
  def exists(p: B => Boolean)(implicit ctx: Scheduler): Boolean
  def forall(p: B => Boolean)(implicit ctx: Scheduler): Boolean
}

