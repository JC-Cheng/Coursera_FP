package quickcheck


import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    oneOf(
      const(empty),
      for {
        v <- arbitrary[Int]
        h <- oneOf(const(empty), genHeap)
      } yield insert(v, h)
    )
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("MinOfAny2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == math.min(a, b)
  }

  // If you insert an element into an empty heap,
  // then delete the minimum, the resulting heap should be empty.
  property("AddAndRemoveEmpty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  // JC
  property("InsertAny") = forAll { (a: Int, h: H) =>
    findMin(insert(a, h)) <= a
  }

  // JC
  property("MeldEmpty") = forAll { h: H =>
    val h0 = meld(h, empty)
    if (isEmpty(h)) isEmpty(h0)
    else findMin(h) == findMin(h0)
  }

  // JC
  property("MoveMin") = forAll { (h1: H, h2: H) =>

    val h3 = meld(h1, h2)
    if (isEmpty(h1)) isEmpty(h2) == isEmpty(h3)
    else {
      val h4 = insert(findMin(h1), h2)
      findMin(h4) == findMin(h3)
    }
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  property("DescendingOrder") = forAll { (h: H) =>
    def iterTest(newHeap: H, prevMin: Int): Boolean = {
      if (isEmpty(newHeap)) true
      else {
        val m = findMin(newHeap)
        (prevMin <= m) && iterTest(deleteMin(newHeap), m)
      }
    }

    if (isEmpty(h)) true
    else {
      val m0 = findMin(h)
      iterTest(deleteMin(h), m0)
    }
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("MergeTwoHeap") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    if (isEmpty(h1) && isEmpty(h2)) isEmpty(h3)
    else {
      val m3 = findMin(h3)
      if (isEmpty(h1)) m3 == findMin(h2)
      else if (isEmpty(h2)) m3 == findMin(h1)
      else m3 == math.min(findMin(h1), findMin(h2))
    }
  }

  // test x in h
  def contains(h: H, x: A): Boolean = {
    if (isEmpty(h)) false
    else {
      val m = findMin(h)
      if (m == x) true
      else contains(deleteMin(h), x)
    }
  }

  // test h0 <= h1
  def isSubTo(h0: H, h1: H): Boolean = {
    if (isEmpty(h0)) true
    else if (isEmpty(h1)) false
    else contains(h1, findMin(h0)) && isSubTo(deleteMin(h0), h1)
  }

  property("Contains") = forAll { (h: H, a: Int) =>
    val h1 = insert(a, h)
    contains(h1, a)
  }

  property("SubHeap") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    isSubTo(h2, h3) && isSubTo(h1, h3)
  }
}
