package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.collection.mutable.ListBuffer

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // Properties:
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // Min of two elements
  property("finding the minimum of two elements") = forAll { (a: Int , b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == Math.min(a, b)
  }

  // Delete the Minimun
  property("delete the minimum,") = forAll { a: Int =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    findMin(h2) == empty
  }

  property("sorted sequence of elements when continually finding and deleting minima") = forAll { h: H =>
    def rec(h: H): List[Int] = h match {
      case h if h == empty => Nil
      case h => findMin(h) :: rec(deleteMin(h))
    }
    val l = rec(h)
    (l, l.tail).zipped.forall(_ <= _)
  }

  property("Melt the Minimum") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    j <- oneOf(empty,genHeap)
  } yield insert(k,empty)

  lazy val genMap: Gen[Map[Int, Int]] =
    const(Map.empty[Int,Int])
    for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
