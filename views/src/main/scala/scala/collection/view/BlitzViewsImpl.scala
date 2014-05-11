package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell
import scala.collection.par.generic.IsReducable
import scala.reflect.ClassTag
import scala.collection.par.workstealing.Arrays.Array2ZippableConvertor
import scala.annotation.implicitNotFound

trait ViewTransformImpl[-A, +B] extends ViewTransform[A, B] { self =>
  //type Fold[A, F] = (A, ResultCell[F]) => ResultCell[F]

  def fold[F](g: Fold[B, F]): Fold[A, F]

  def >>[C](next: ViewTransform[B, C]) = new ViewTransformImpl[A, C] {
    def fold[F](fd: Fold[C, F]): Fold[A, F] = self.fold(next.fold(fd))
  }
}

object ViewTransforms {
  class Identity[A] extends ViewTransformImpl[A, A] {
    def fold[F](fd: Fold[A, F]): Fold[A, F] = fd
  }

  class Map[A, B](m: A => B) extends ViewTransformImpl[A, B] {
    def fold[F](fd: Fold[B, F]): Fold[A, F] =
      (x, acc) => fd(m(x), acc)
  }

  class Filter[A](p: A => Boolean) extends ViewTransformImpl[A, A] {
    def fold[F](fd: Fold[A, F]): Fold[A, F] =
      (x, acc) => if (p(x)) fd(x, acc) else acc
  }
}

trait BlitzViewImpl[B] extends BlitzView[B] { self =>
  /* methods: V -> V */
  override def drop(n: Int): BlitzView[B] = ???
  override def take(n: Int): BlitzView[B] = ???

  /* methods: V -> other array structure */
  override def toArray()(implicit classtag: ClassTag[B], ctx: Scheduler): Array[B] = {
    val tmp = toList_().toArray
    val sz = tmp.size - 1
    val rng = 0 to (sz / 2)
    def swap(x: Int) = {
      val t = tmp(x)
      tmp(x) = tmp(sz - x)
      tmp(sz - x) = t
    }
    rng.toPar.foreach(swap(_))
    tmp
  }

  private[this] def toList_()(implicit ctx: Scheduler): List[B] =
    aggregate(Nil: List[B])((x, xs) => x :: xs)((xs, ys) => ys ++ xs)

  override def toList()(implicit ctx: Scheduler): List[B] =
    toList_().reverse

  /* methods: V -> V[constant type] */
  override def toInts(implicit f: Numeric[B]): BlitzView[Int] = map(f.toInt(_))
  override def toDoubles(implicit f: Numeric[B]): BlitzView[Double] = map(f.toDouble(_))
  override def toFloats(implicit f: Numeric[B]): BlitzView[Float] = map(f.toFloat(_))
  override def toLongs(implicit f: Numeric[B]): BlitzView[Long] = map(f.toLong(_))

  /* methods: V -> 1 */
  override def reduce(op: (B, B) => B)(implicit ctx: Scheduler): B =
    reduceOpt(op)(ctx).get // throws an Exception if empty
  override def min()(implicit ord: Ordering[B], ctx: Scheduler): B =
    minOpt()(ord, ctx).get // throws an Exception if empty
  override def max()(implicit ord: Ordering[B], ctx: Scheduler): B =
    maxOpt()(ord, ctx).get // throws an Exception if empty
  override def sum()(implicit num: Numeric[B], ctx: Scheduler): B =
    aggregate(num.zero)(num.plus(_, _))(num.plus(_, _))
  override def product()(implicit num: Numeric[B], ctx: Scheduler): B =
    aggregate(num.one)(num.times(_, _))(num.times(_, _))
}

object View {
  def apply[T, Repr](xss: Par[Repr])(implicit conv: IsReducable[Repr, T]): BlitzView[T] = new BlitzViewC[T] {
    type A = T
    val xs = conv(xss)
    def transform = new ViewTransforms.Identity()
  }

  def apply[T, T1 <: T, T2 <: T](xss: BlitzView[T1], yss: BlitzView[T2]): BlitzView[T] = new BlitzViewVV[T] {
    type A = T
    val xs = xss.asInstanceOf[BlitzView[T]]
    val ys = yss.asInstanceOf[BlitzView[T]]
    def transform = new ViewTransforms.Identity()
  }

  def range(x: Int, y: Int)(implicit ctx: Scheduler): BlitzView[Int] = {
    val xs = x until (y+1)
    apply(xs.toPar)(rangeIsZippable(ctx))
  }

  def of[T](xss: T*)(implicit conv: IsReducable[Array[T], T], c: ClassTag[T]): BlitzView[T] = new BlitzViewC[T] {
    type A = T
    val xs = conv(xss.toArray.toPar)
    def transform = new ViewTransforms.Identity()
  }

  /** Decorator example */
  implicit def addFlatten[S, B <: BlitzView[S]](view: BlitzView[B]) =
    new ViewWithFlatten[S, B](view)

  class ViewWithFlatten[S, B <: BlitzView[S]] (val view: BlitzView[B]) extends AnyVal {
    def flatten: BlitzView[S] = ???
  }
}

object ViewUtils {
  // equivalent to liftA2 for the Option[_] Functor
  def optCombine[A](f: (A, A) => A)(ox: Option[A], oy: Option[A]): Option[A] =
    (ox, oy) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => ox.orElse(oy)
    }
}
