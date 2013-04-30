package scala.collection.parallel
package scalatest



import org.scalatest._
import Conc._



class ConcTest extends FunSuite {

  import scala.collection._

  def testTraverse(c: Conc[Int], comparison: Seq[Int]) {
    val elems = mutable.ArrayBuffer[Int]()
    def traverse(c: Conc[Int]): Unit = c match {
      case Zero => // done
      case Single(x) => elems += x
      case Chunk(xs, sz) => elems ++= xs.take(sz)
      case left <> right => traverse(left); traverse(right)
    }

    traverse(c)
    assert(elems == comparison)
  }

  def testBalance[T](c: Conc[T]) {
    val level = c.level
    val size = c.size
    val depthBound = 4 * math.log(size) / math.log(2)
    assert(level < depthBound)
  }

  test("<> Single trees") {
    val size = 20000
    val elems = 0 until size
    var conc: Conc[Int] = Zero
    for (i <- elems) conc = conc <> Single(i)

    testTraverse(conc, elems)
    testBalance(conc)
  }

  test("<> trees") {
    val size = 20000
    val elems1 = 0 until size
    val elems2 = size until (10 * size)
    var conc1: Conc[Int] = Zero
    for (i <- elems1) conc1 = conc1 <> Single(i)
    var conc2: Conc[Int] = Zero
    for (i <- elems2) conc2 = conc2 <> Single(i)
    val conc = conc1 <> conc2

    testTraverse(conc, elems1 ++ elems2)
    testBalance(conc)
  }

  test("<> elems") {
    val size = 20000
    val elems = 0 until size
    var conc: Conc[Int] = Zero
    for (i <- elems) conc = conc <> i

    testTraverse(conc, elems)
    testBalance(conc)
  }

  test("Buffer.+=") {
    val size = 200000
    val elems = 0 until size
    val cb = new Conc.Buffer[Int]
    for (i <- elems) cb += i
    val conc = cb.result

    testTraverse(conc, elems)
  }

}








