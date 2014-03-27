package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell
import scala.collection.parallel.ParSeq
import scala.collection.par.generic.IsReducable

abstract class ViewTransform[-A, +B] {
  self =>
  type Fold[A, F] = (A, ResultCell[F]) => ResultCell[F]

  def fold[F](g: Fold[B, F]): Fold[A, F]

  def >>[C](next: ViewTransform[B, C]) = new ViewTransform[A, C] {
    def fold[F](fd: Fold[C, F]): Fold[A, F] = self.fold(next.fold(fd))
  }
}

trait BlitzView[B] { self =>
  type A // type of source list

  def transform: ViewTransform[A, B] // stack of transforms

  def >>[C](next: ViewTransform[B, C]): BlitzView[C]

  def map[C](f: B => C): BlitzView[C]
  def filter(p: B => Boolean): BlitzView[B]
  def reduce[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R
  def count()(implicit ctx: Scheduler): Int
  def min()(implicit ord: Ordering[B], ctx: Scheduler): Option[B]
  def max()(implicit ord: Ordering[B], ctx: Scheduler): Option[B]
}

