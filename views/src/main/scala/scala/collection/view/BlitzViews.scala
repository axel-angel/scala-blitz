package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell
import scala.reflect.ClassTag

trait BlitzView[+B] {
  /* operators */
  def ++[A >: B](ys: BlitzView[A]): BlitzView[A]
  def :::[A >: B](ys: BlitzView[A]): BlitzView[A]
  def ::[A >: B](y: A): BlitzView[A]

  /* methods: V -> V */
  def flatMap[C](f: B => Array[C])(implicit ctx: Scheduler): BlitzView[C]
  def map[C](f: B => C): BlitzView[C]
  def filter(p: B => Boolean): BlitzView[B]
  def drop(n: Int): BlitzView[B]
  def take(n: Int): BlitzView[B]

  /* methods: V -> other array structure */
  def toArray[A >: B]()(implicit ct: ClassTag[B], ctx: Scheduler): Array[A]
  def toList[A >: B]()(implicit ctx: Scheduler): List[A]

  /* methods: V -> V[constant type] */
  def toInts[A >: B](implicit f: Numeric[A]): BlitzView[Int]
  def toDoubles[A >: B](implicit f: Numeric[A]): BlitzView[Double]
  def toFloats[A >: B](implicit f: Numeric[A]): BlitzView[Float]
  def toLongs[A >: B](implicit f: Numeric[A]): BlitzView[Long]

  /* methods: V -> 1 */
  def reduceOpt[A >: B](op: (A, A) => A)(implicit ctx: Scheduler): Option[A]
  def reduce[A >: B](op: (A, A) => A)(implicit ctx: Scheduler): A
  def aggregate[R, A >: B](z: => R)(op: (A, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R
  def minOpt[A >: B]()(implicit ord: Ordering[A], ctx: Scheduler): Option[A]
  def maxOpt[A >: B]()(implicit ord: Ordering[A], ctx: Scheduler): Option[A]
  def min[A >: B]()(implicit ord: Ordering[A], ctx: Scheduler): A
  def max[A >: B]()(implicit ord: Ordering[A], ctx: Scheduler): A
  def sum[A >: B]()(implicit num: Numeric[A], ctx: Scheduler): A
  def product[A >: B]()(implicit num: Numeric[A], ctx: Scheduler): A

  /* methods: V -> 1[constant type] */
  def size()(implicit ctx: Scheduler): Int
  def count(p: B => Boolean)(implicit ctx: Scheduler): Int
  def find(p: B => Boolean)(implicit ctx: Scheduler): Option[B]
  def exists(p: B => Boolean)(implicit ctx: Scheduler): Boolean
  def forall(p: B => Boolean)(implicit ctx: Scheduler): Boolean
}

