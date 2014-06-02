package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell
import scala.reflect.ClassTag

trait BlitzView[B] {
  /* operators */
  def ++(ys: BlitzView[B]): BlitzView[B]
  def :::(ys: BlitzView[B]): BlitzView[B]
  def ::(y: B): BlitzView[B]

  /* methods: V -> V */
  def flatMap[C](f: B => Array[C])(implicit ctx: Scheduler): BlitzView[C]
  def map[C](f: B => C): BlitzView[C]
  def filter(p: B => Boolean): BlitzView[B]
  def drop(n: Int): BlitzView[B]
  def take(n: Int): BlitzView[B]

  /* methods: V -> other array structure */
  def toArray()(implicit classtag: ClassTag[B], ctx: Scheduler): Array[B]
  def toList()(implicit ctx: Scheduler): List[B]

  /* methods: V -> V[constant type] */
  def toInts(implicit f: Numeric[B]): BlitzView[Int]
  def toDoubles(implicit f: Numeric[B]): BlitzView[Double]
  def toFloats(implicit f: Numeric[B]): BlitzView[Float]
  def toLongs(implicit f: Numeric[B]): BlitzView[Long]

  /* methods: V -> 1 */
  def reduceOpt(op: (B, B) => B)(implicit ctx: Scheduler): Option[B]
  def reduce(op: (B, B) => B)(implicit ctx: Scheduler): B
  def aggregate[R](z: => R)(op: (B, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R
  def minOpt()(implicit ord: Ordering[B], ctx: Scheduler): Option[B]
  def maxOpt()(implicit ord: Ordering[B], ctx: Scheduler): Option[B]
  def min()(implicit ord: Ordering[B], ctx: Scheduler): B
  def max()(implicit ord: Ordering[B], ctx: Scheduler): B
  def sum()(implicit num: Numeric[B], ctx: Scheduler): B
  def product()(implicit num: Numeric[B], ctx: Scheduler): B

  /* methods: V -> 1[constant type] */
  def size()(implicit ctx: Scheduler): Int
  def count(p: B => Boolean)(implicit ctx: Scheduler): Int
  def find(p: B => Boolean)(implicit ctx: Scheduler): Option[B]
  def exists(p: B => Boolean)(implicit ctx: Scheduler): Boolean
  def forall(p: B => Boolean)(implicit ctx: Scheduler): Boolean
}

