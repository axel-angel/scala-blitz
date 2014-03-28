package scala.collection.views

import scala.collection.par._
import workstealing.ResultCell
import scala.collection.par.generic.IsReducable
import scala.reflect.ClassTag


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

object View {
  def apply[T, Repr](xss: Par[Repr])(implicit conv: IsReducable[Repr, T]): BlitzView[T] = new BlitzViewC[T] {
    type A = T
    val xs = conv(xss)
    def transform = new ViewTransforms.Identity()
  }

  def apply[T, Repr](xss: Par[Repr], yss: Par[Repr])(implicit conv: IsReducable[Repr, T]): BlitzView[T] = new BlitzViewCC[T] {
    type A = T
    val xs = conv(xss)
    val ys = conv(yss)
    def transform = new ViewTransforms.Identity()
  }

  /* // FIXME: not implemented yet
  def apply[T](gfunc: Int => T): BlitzView[T] = new BlitzViewG[T] {
    type A = T
    val g = gfunc
    def transform = new ViewTransforms.Identity()
  }
  */

  def range(x: Int, y: Int)(implicit ctx: Scheduler): BlitzView[Int] = {
    val xs = x until (y+1)
    apply(xs.toPar)(rangeIsZippable(ctx))
  }

  def of[T](xss: T*)(implicit conv: IsReducable[Array[T], T], c: ClassTag[T]): BlitzView[T] = new BlitzViewC[T] {
    type A = T
    val xs = conv(xss.toArray.toPar)
    def transform = new ViewTransforms.Identity()
  }
}
