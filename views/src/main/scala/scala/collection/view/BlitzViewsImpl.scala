package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell
import scala.collection.par.generic.IsReducable
import scala.reflect.ClassTag
import scala.collection.par.workstealing.Arrays.Array2ZippableConvertor
import scala.collection.mutable.{HashMap => MHashMap, HashSet => MHashSet}
import scala.collection.immutable.{HashSet, HashMap}

trait ViewTransform[-A, +B] { self =>
  type Fold[A, F] = (A, ResultCell[F]) => ResultCell[F]

  def fold[F](g: Fold[B, F]): Fold[A, F]

  def >>[C](next: ViewTransform[B, C]) = new ViewTransform[A, C] {
    def fold[F](fd: Fold[C, F]): Fold[A, F] = self.fold(next.fold(fd))
  }
}

object ViewTransforms {
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
}

trait BlitzViewImpl[+B] extends BlitzView[B] { self =>
  /* internals */
  def >>[C](next: ViewTransform[B, C]): BlitzView[C]
  def genericInvoke[R](op: (B, ResultCell[R]) => ResultCell[R], pstop: ResultCell[R] => Boolean)(reducer: (R, R) => R)(implicit ctx: Scheduler): ResultCell[R]


  /* operators */
  override def ++[A >: B](y: BlitzView[A]): BlitzView[A] = new BlitzViewVV[A] {
      val xs = self
      val ys = y
  }

  override def :::[A >: B](ys: BlitzView[A]): BlitzView[A] = ys ++ self
  override def ::[A >: B](y: A) =
    View.singleton(y) ++ self

  /* methods: V -> V */
  override def flatMap[C, U](f: B => U)(implicit ctx: Scheduler, viewable: BlitzView.IsViewable[U, C]) = {
    def flatMapper(x: B): BlitzView[C] = viewable(f(x))
    new BlitzViewFlattenVs[C, BlitzView, BlitzView] {
      val zss = self >> new ViewTransforms.Map[B,BlitzView[C]](flatMapper)

    }
  }

  override def map[C](f: B => C) = self >> new ViewTransforms.Map[B,C](f)
  override def filter(p: B => Boolean) = self >> new ViewTransforms.Filter[B](p)


  /* methods: V -> other array structure */
  override def toArray[A >: B]()(implicit ct: ClassTag[A], ctx: Scheduler): Array[A] = {
    val tmp = toList_[A]().toArray[A]
    val sz = tmp.size - 1
    if (sz < 0) return tmp
    val rng = 0 to (sz / 2)
    def swap(x: Int) = {
      val t = tmp(x)
      tmp(x) = tmp(sz - x)
      tmp(sz - x) = t
    }
    rng.toPar.foreach(swap(_))
    tmp
  }

  private[this] def toList_[A >: B]()(implicit ctx: Scheduler): List[A] =
    aggregate(Nil: List[A])((x, xs) => x :: xs)((xs, ys) => ys ++ xs)

  override def toList[A >: B]()(implicit ctx: Scheduler): List[A] =
    toList_().reverse


  /* methods: V -> V[constant type] */
  override def toInts[A >: B](implicit f: Numeric[A]): BlitzView[Int] =
    map(f.toInt(_))
  override def toDoubles[A >: B](implicit f: Numeric[A]): BlitzView[Double] =
    map(f.toDouble(_))
  override def toFloats[A >: B](implicit f: Numeric[A]): BlitzView[Float] =
    map(f.toFloat(_))
  override def toLongs[A >: B](implicit f: Numeric[A]): BlitzView[Long] =
    map(f.toLong(_))


  /* methods: V -> 1 */
  override def aggregate[R, A >: B](z: => R)(op: (A, R) => R)(reducer: (R, R) => R)(implicit ctx: Scheduler): R = {
    def folder(x: B, cell: ResultCell[R]): ResultCell[R] = {
      cell.result = op(x, if (cell.isEmpty) z else cell.result)
      cell
    }
    val stopper = ViewUtils.neverStop[B,R]_
    genericInvoke(folder, stopper)(reducer)(ctx).toOption.getOrElse(z)
  }

  override def reduceOpt[A >: B](op: (A, A) => A)(implicit ctx: Scheduler): Option[A] = {
    def folder(x: A, cell: ResultCell[A]): ResultCell[A] = {
      cell.result = if (cell.isEmpty) x else op(x, cell.result)
      cell
    }
    genericInvoke(folder, ViewUtils.neverStop[A,A])(op)(ctx).toOption
  }

  override def find(p: B => Boolean)(implicit ctx: Scheduler): Option[B] = {
    def folder(x: B, rc: ResultCell[B]): ResultCell[B] = {
      if (rc.isEmpty && p(x)) rc.result = x
      rc
    }
    genericInvoke(folder, ViewUtils.nonEmptyStop[B,B])((x, y) => x).toOption
  }

  override def exists(p: B => Boolean)(implicit ctx: Scheduler): Boolean = {
    def folder(x: B, rc: ResultCell[Boolean]): ResultCell[Boolean] = {
      if (p(x)) rc.result = true
      rc
    }
    val stopper = ViewUtils.equalStop(true)_
    genericInvoke(folder, stopper)(_ || _).toOption.getOrElse(false)
  }

  override def size()(implicit ctx: Scheduler): Int =
    aggregate(0)((_:B, x: Int) => x+1)(_ + _)(ctx)
  override def count(p: B => Boolean)(implicit ctx: Scheduler): Int =
    aggregate(0)((x: B, c: Int) => c  + (if (p(x)) 1 else 0))(_ + _)
  override def minOpt[A >: B]()(implicit ord: Ordering[A], ctx: Scheduler): Option[A] =
    reduceOpt((x: B, y: B) => if (ord.lt(x,y)) x else y)(ctx)
  override def maxOpt[A >: B]()(implicit ord: Ordering[A], ctx: Scheduler): Option[A] =
    minOpt[A]()(ord.reverse, ctx)
  override def forall(p: B => Boolean)(implicit ctx: Scheduler): Boolean =
    !exists(x => !p(x))(ctx)
  override def reduce[A >: B](op: (A, A) => A)(implicit ctx: Scheduler): A =
    reduceOpt(op)(ctx).get // throws an Exception if empty
  override def min[A >: B]()(implicit ord: Ordering[A], ctx: Scheduler): A =
    minOpt[A]()(ord, ctx).get // throws an Exception if empty
  override def max[A >: B]()(implicit ord: Ordering[A], ctx: Scheduler): A =
    maxOpt[A]()(ord, ctx).get // throws an Exception if empty
  override def sum[A >: B]()(implicit num: Numeric[A], ctx: Scheduler): A =
    aggregate(num.zero)(num.plus(_, _))(num.plus(_, _))
  override def product[A >: B]()(implicit num: Numeric[A], ctx: Scheduler): A =
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
    val xs = xss
    val ys = yss
    def transform = new ViewTransforms.Identity()
  }

  def range(x: Int, y: Int)(implicit ctx: Scheduler): BlitzView[Int] = {
    val xs = x to y
    apply(xs.toPar)(rangeIsZippable(ctx))
  }

  def of[T](xss: T*)(implicit conv: IsReducable[Array[T], T], c: ClassTag[T]): BlitzView[T] = new BlitzViewC[T] {
    type A = T
    val xs = conv(xss.toArray.toPar)
    def transform = new ViewTransforms.Identity()
  }

  def singleton[T](_x: T): BlitzView[T] = new BlitzViewS[T] {
    type A = T
    val x = _x
    def transform = new ViewTransforms.Identity()
  }
}

object ViewUtils {
  // equivalent to liftA2 for the Option[_] Functor
  def optCombine[A](f: (A, A) => A)(ox: Option[A], oy: Option[A]): Option[A] =
    (ox, oy) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => ox.orElse(oy)
    }

  // This reuse the left ResultCell
  def rcCombine[@specialized A](f: (A, A) => A)
    (rcx: ResultCell[A], rcy: ResultCell[A]): ResultCell[A] =
    {
      if (rcx.isEmpty) rcy
      else if (rcy.isEmpty) rcx
      else {
        rcx.result = f(rcx.result, rcy.result)
        rcx
      }
    }

  // Declare useful stoppers for genericInvoke stoppable function
  def toStopper[T,R](pstop: ResultCell[R] => Boolean)
    (x: T, rc: ResultCell[R]): Boolean = pstop(rc)
  def neverStop[T,R](rc: ResultCell[R]): Boolean = false
  def nonEmptyStop[T,R](rc: ResultCell[R]): Boolean = !rc.isEmpty
  def equalStop[T,R](v: R)(rc: ResultCell[R]): Boolean =
    !rc.isEmpty && rc.result == v
}

/* Provides conversions from collections to our specialized BlitzViewImpl.
 * This is based on implicit evidence, see toViewable.
 */
object BlitzViewImpl {
  import BlitzView._

  /* provides a way to lift BlitzView to BlitzViewImpl (compile-time checked) */
  private[views] implicit def internalAPI[T[_] <: BlitzView[_], EL](x: T[EL]): BlitzViewImpl[EL] = x.asInstanceOf[BlitzViewImpl[EL]]

  /* BlitzView is Viewable */

  implicit def bviewIsViewable[T] =
    new IsViewable[BlitzView[T], T] {
      override def apply(v: BlitzView[T]): BlitzView[T] = v
    }

  /* Specialized simple data lists */

  implicit def arrayIsViewable[T](implicit ctx: Scheduler) =
    new IsViewable[Array[T], T] {
      override def apply(c: Array[T]): BlitzView[T] = {
        View(c.toPar)(new Array2ZippableConvertor)
      }
    }

  implicit def rangeIsViewable[L <: Range](implicit ctx: Scheduler) =
    new IsViewable[L, Int] {
      override def apply(c: L): BlitzView[Int] = {
        View(c.toPar)(rangeIsZippable)
      }
    }


  /* Specialized faster Set/Map implementation */

  implicit def hashsetIsViewable[T](implicit ctx: Scheduler, ct: ClassTag[T]) =
    new IsViewable[HashSet[T], T] {
      override def apply(c: HashSet[T]): BlitzView[T] = {
        View(c.toPar)(hashTrieSetIsReducable)
      }
    }

  implicit def hashmapIsViewable[K,V](implicit ctx: Scheduler, ct1: ClassTag[Tuple2[K,V]]) =
    new IsViewable[HashMap[K,V], (K,V)] {
      override def apply(c: HashMap[K,V]): BlitzView[(K,V)] = {
        View(c.toPar)(hashTrieMapIsReducable)
      }
    }


  /* More general slower Set/Map implementation */

  implicit def setIsViewable[T](implicit ctx: Scheduler, ct: ClassTag[T]) =
    new IsViewable[Set[T], T] {
      override def apply(c: Set[T]): BlitzView[T] = {
        View(c.toArray.toPar)(new Array2ZippableConvertor)
      }
    }

  implicit def mapIsViewable[K,V](implicit ctx: Scheduler, ct1: ClassTag[Tuple2[K,V]]) =
    new IsViewable[Map[K,V], (K,V)] {
      override def apply(c: Map[K,V]): BlitzView[(K,V)] = {
        View(c.toArray.toPar)(new Array2ZippableConvertor)
      }
    }


  /* Mutable Set/Map data structures */

  implicit def mHashSetIsViewable[T](implicit ctx: Scheduler) =
    new IsViewable[MHashSet[T], T] {
      override def apply(c: MHashSet[T]): BlitzView[T] = {
        View(c.toPar)(hashSetIsReducable)
      }
    }

  implicit def mHashMapIsViewable[K,V](implicit ctx: Scheduler) =
    new IsViewable[MHashMap[K,V], (K,V)] {
      override def apply(c: MHashMap[K,V]): BlitzView[(K,V)] = {
        View(c.toPar)(hashMapIsReducable)
      }
    }


  /* Option */

  implicit def optionIsViewable[T, U <: Option[T]](implicit conv: U => Option[T]) =
    new IsViewable[U, T] {
      override def apply(_ox: U) = {
        new BlitzViewO[T] {
          type A = T
          val ox = conv(_ox)
          def transform = new ViewTransforms.Identity()
        }
      }
    }


  /* Provides implicit-extension for 'flatten' on nested Views */

  implicit def addFlatten[U, B[_] <: BlitzView[_], C[_] <: BlitzView[_]](view: B[C[U]]) =
    new ViewWithFlatten[U, B, C](view)

  class ViewWithFlatten[U, B[_] <: BlitzView[_], C[_] <: BlitzView[_]](val view: B[C[U]]) extends AnyVal {
    def flatten()(implicit ctx: Scheduler, ct: ClassTag[U]): BlitzView[U] =
      new BlitzViewFlattenVs[U, B, C] {
        val zss = view
      }
  }
}

