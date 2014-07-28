package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell
import scala.reflect.ClassTag
import scala.annotation.implicitNotFound

/*
 * BlitzView: Lightweight, non-strict and parallel-efficient views (prototype).
 *
 * Guarantee constant time and constant memory for transformer operations.
 * Based on Scala Blitz for configurable parallelism.
 */
trait BlitzView[+B] { self: BlitzViewImpl[B] =>
  /* operators */
  def ++[A >: B](ys: BlitzView[A]): BlitzView[A]
  def :::[A >: B](ys: BlitzView[A]): BlitzView[A]
  def ::[A >: B](y: A): BlitzView[A]

  /* methods (transformers): V -> V */
  def flatMap[C, U](f: B => U)(implicit ctx: Scheduler, viewable: BlitzView.IsViewable[U, C]): BlitzView[C]
  def map[C](f: B => C): BlitzView[C]
  def filter(p: B => Boolean): BlitzView[B]

  /* methods: V -> other array structure */
  def toArray[A >: B]()(implicit ct: ClassTag[A], ctx: Scheduler): Array[A]
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


/*
 * We provide implicit conversions for certain native types. The user can call
 * bview on the supported classes, the implicit IsViewable for this type kicks
 * in and attach the bview method to it by converting the type to Viewable.
 *
 * As follows: toViewable[L, A] -> search for an implicit IsViewable[L, A]
 * then: get the appropriate implicit, for eg: arrayIsViewable
 * this requires an implicit Scheduler in scope, given this one, we can create
 * an IsViewable instance that can be used as an evidence applied in toViewable.
 *
 * toViewable <- arrayIsViewable (<- Scheduler)
 *  \-> IsViewable -> Viewable with bview
 */
object BlitzView {
  @implicitNotFound("cannot find a valid conversion from ${L} to BlitzView")
  trait IsViewable[L, A] {
    def apply(c: L): BlitzView[A]
  }

  implicit def toViewable[L, A](col: L)(implicit evidence: IsViewable[L, A]) =
    new Viewable[L, A](col)(evidence)

  class Viewable[L, A](col: L)(evidence: IsViewable[L, A]) {
    def bview: BlitzView[A] = evidence.apply(col)
  }
}
