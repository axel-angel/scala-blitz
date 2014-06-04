package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell
import scala.collection.par.generic.IsReducable
import scala.reflect.ClassTag
import scala.collection.par.workstealing.Arrays.Array2ZippableConvertor
import scala.annotation.implicitNotFound
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
  def >>[C](next: ViewTransform[B, C]): BlitzViewImpl[C]
  def aggInternal[R](op: (B, ResultCell[R]) => ResultCell[R], pstop: ResultCell[R] => Boolean)(reducer: (R, R) => R)(implicit ctx: Scheduler): ResultCell[R]


  /* operators */
  override def ++[A >: B](y: BlitzView[A]): BlitzView[A] = y match {
    case _ys: BlitzViewImpl[B] => new BlitzViewVV[B] {
      val xs = self
      val ys = _ys
    }
    case _ => sys.error("operation not implemented")
  }
  override def :::[A >: B](ys: BlitzView[A]): BlitzView[A] = ys ++ self
  override def ::[A >: B](y: A) =
    View.singleton(y) ++ self

  /* methods: V -> V */
  override def flatMap[C](f: B => Array[C])(implicit ctx: Scheduler) = {
    def flatMapper(x: B): BlitzViewImpl[C] = Scope.arrayIsViewable(ctx)(f(x))
    new BlitzViewFlattenVs[C] {
      val zss = self >> new ViewTransforms.Map[B,BlitzViewImpl[C]](flatMapper)
    }
  }
  override def map[C](f: B => C) = self >> new ViewTransforms.Map[B,C](f)
  override def filter(p: B => Boolean) = self >> new ViewTransforms.Filter[B](p)
  override def drop(n: Int): BlitzView[B] = ??? // TODO
  override def take(n: Int): BlitzView[B] = ??? // TODO


  /* methods: V -> other array structure */
  override def toArray[A >: B]()(implicit ct: ClassTag[B], ctx: Scheduler): Array[A] = {
    val tmp = toList_().toArray
    val sz = tmp.size - 1
    if (sz < 0) return tmp.asInstanceOf[Array[A]] // TODO: ?
    val rng = 0 to (sz / 2)
    def swap(x: Int) = {
      val t = tmp(x)
      tmp(x) = tmp(sz - x)
      tmp(sz - x) = t
    }
    rng.toPar.foreach(swap(_))
    tmp.asInstanceOf[Array[A]] // TODO: ?
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
    aggInternal(folder, stopper)(reducer)(ctx).toOption.getOrElse(z)
  }

  override def reduceOpt[A >: B](op: (A, A) => A)(implicit ctx: Scheduler): Option[A] = {
    def folder(x: A, cell: ResultCell[A]): ResultCell[A] = {
      cell.result = if (cell.isEmpty) x else op(x, cell.result)
      cell
    }
    aggInternal(folder, ViewUtils.neverStop[A,A])(op)(ctx).toOption
  }

  override def find(p: B => Boolean)(implicit ctx: Scheduler): Option[B] = {
    def folder(x: B, rc: ResultCell[B]): ResultCell[B] = {
      if (rc.isEmpty && p(x)) rc.result = x
      rc
    }
    aggInternal(folder, ViewUtils.nonEmptyStop[B,B])((x, y) => x).toOption
  }

  override def exists(p: B => Boolean)(implicit ctx: Scheduler): Boolean = {
    def folder(x: B, rc: ResultCell[Boolean]): ResultCell[Boolean] = {
      if (p(x)) rc.result = true
      rc
    }
    val stopper = ViewUtils.equalStop(true)_
    aggInternal(folder, stopper)(_ || _).toOption.getOrElse(false)
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
  def apply[T, Repr](xss: Par[Repr])(implicit conv: IsReducable[Repr, T]): BlitzViewImpl[T] = new BlitzViewC[T] {
    type A = T
    val xs = conv(xss)
    def transform = new ViewTransforms.Identity()
  }

  def apply[T, T1 <: T, T2 <: T](xss: BlitzViewImpl[T1], yss: BlitzViewImpl[T2]): BlitzViewImpl[T] = new BlitzViewVV[T] {
    type A = T
    val xs = xss.asInstanceOf[BlitzViewImpl[T]]
    val ys = yss.asInstanceOf[BlitzViewImpl[T]]
    def transform = new ViewTransforms.Identity()
  }

  def range(x: Int, y: Int)(implicit ctx: Scheduler): BlitzViewImpl[Int] = {
    val xs = x until (y+1)
    apply(xs.toPar)(rangeIsZippable(ctx))
  }

  def of[T](xss: T*)(implicit conv: IsReducable[Array[T], T], c: ClassTag[T]): BlitzViewImpl[T] = new BlitzViewC[T] {
    type A = T
    val xs = conv(xss.toArray.toPar)
    def transform = new ViewTransforms.Identity()
  }

  def singleton[T](_x: T): BlitzViewImpl[T] = new BlitzViewS[T] {
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

  def toStopper[T,R](pstop: ResultCell[R] => Boolean)
    (x: T, rc: ResultCell[R]): Boolean = pstop(rc)
  def neverStop[T,R](rc: ResultCell[R]): Boolean = false
  def nonEmptyStop[T,R](rc: ResultCell[R]): Boolean = !rc.isEmpty
  def equalStop[T,R](v: R)(rc: ResultCell[R]): Boolean =
    !rc.isEmpty && rc.result == v
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
object Scope {
  implicit def toViewable[L, A](col: L)(implicit evidence: IsViewable[L, A]) =
    new Viewable[L, A](col)(evidence)

  @implicitNotFound("cannot find a valid conversion from ${L} to BlitzView")
  trait IsViewable[L, A] {
    def apply(c: L): BlitzViewImpl[A]
  }

  class Viewable[L, A](col: L)(evidence: IsViewable[L, A]) {
    def bview: BlitzViewImpl[A] = evidence.apply(col)
  }

  /* Specialized simple data lists */

  implicit def arrayIsViewable[T](implicit ctx: Scheduler) =
    new IsViewable[Array[T], T] {
      override def apply(c: Array[T]): BlitzViewImpl[T] = {
        View(c.toPar)(new Array2ZippableConvertor)
      }
    }

  implicit def rangeIsViewable[L <: Range](implicit ctx: Scheduler) =
    new IsViewable[L, Int] {
      override def apply(c: L): BlitzViewImpl[Int] = {
        View(c.toPar)(rangeIsZippable)
      }
    }


  /* Specialized faster Set/Map implementation */

  implicit def hashsetIsViewable[T](implicit ctx: Scheduler, ct: ClassTag[T]) =
    new IsViewable[HashSet[T], T] {
      override def apply(c: HashSet[T]): BlitzViewImpl[T] = {
        View(c.toPar)(hashTrieSetIsReducable)
      }
    }

  implicit def hashmapIsViewable[K,V](implicit ctx: Scheduler, ct1: ClassTag[Tuple2[K,V]]) =
    new IsViewable[HashMap[K,V], (K,V)] {
      override def apply(c: HashMap[K,V]): BlitzViewImpl[(K,V)] = {
        View(c.toPar)(hashTrieMapIsReducable)
      }
    }


  /* More general slower Set/Map implementation */

  // TODO: should we convert to Array, for immutable, stays correct
  // Or we specialize with hashSetIsReducable (cf: workstealing)
  // TODO: how to factorize with arrayIsViewable?
  implicit def setIsViewable[T](implicit ctx: Scheduler, ct: ClassTag[T]) =
    new IsViewable[Set[T], T] {
      override def apply(c: Set[T]): BlitzViewImpl[T] = {
        View(c.toArray.toPar)(new Array2ZippableConvertor)
      }
    }

  // TODO: same as setIsViewable
  implicit def mapIsViewable[K,V](implicit ctx: Scheduler, ct1: ClassTag[Tuple2[K,V]]) =
    new IsViewable[Map[K,V], (K,V)] {
      override def apply(c: Map[K,V]): BlitzViewImpl[(K,V)] = {
        View(c.toArray.toPar)(new Array2ZippableConvertor)
      }
    }


  /* Mutable Set/Map data structures */

  implicit def mHashSetIsViewable[T](implicit ctx: Scheduler) =
    new IsViewable[MHashSet[T], T] {
      override def apply(c: MHashSet[T]): BlitzViewImpl[T] = {
        View(c.toPar)(hashSetIsReducable)
      }
    }

  implicit def mHashMapIsViewable[K,V](implicit ctx: Scheduler) =
    new IsViewable[MHashMap[K,V], (K,V)] {
      override def apply(c: MHashMap[K,V]): BlitzViewImpl[(K,V)] = {
        View(c.toPar)(hashMapIsReducable)
      }
    }


  /* Optional */

  implicit def optionIsViewable[T] =
    new IsViewable[Option[T], T] {
      override def apply(_ox: Option[T]) = {
        new BlitzViewO[T] {
          type A = T
          val ox = _ox
          def transform = new ViewTransforms.Identity()
        }
      }
    }


  /* Provides 'flatten' on nested Views */

  implicit def addFlatten[U, B <: BlitzViewImpl[BlitzViewImpl[U]]](view: B) =
    new ViewWithFlatten[U, B](view)

  class ViewWithFlatten[U, B <: BlitzViewImpl[BlitzViewImpl[U]]](val view: B) extends AnyVal {
    def flatten()(implicit ctx: Scheduler, ct: ClassTag[U]): BlitzView[U] =
      new BlitzViewFlattenVs[U] {
        val zss = view
      }
  }
}

