package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.*

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a, h)

  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("empty") = forAll { (a: A) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("min1") = forAll { (a: A) =>
    findMin(insert(a, empty)) == a
  }

  property("min2") = forAll { (a: A, b: A) =>
    val h = insert(b, insert(a, empty))
    if (a > b) findMin(h) == b
    else findMin(h) == a
  }

  property("minMeld") = forAll { (a: H, b: H) =>
    if (isEmpty(a))
      if (isEmpty(b))
        isEmpty(meld(a, b))
      else findMin(b) == findMin(meld(a, b))
    else if (isEmpty(b))
      findMin(a) == findMin(meld(a, b))
    else findMin(meld(a, b)) == findMin(a) || findMin(meld(a, b)) == findMin(b)
  }

  property("sorted") = forAll { (h: H) =>
    @tailrec
    def checkSorted(h: H, prev: A): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        if (m < prev) false
        else checkSorted(deleteMin(h), m)
      }
    if (isEmpty(h)) true
    else checkSorted(h, findMin(h))
  }

  property("sorted2") = forAll { (a: H, b: H) =>
    @tailrec
    def drainTo(a: H, b: H): (H, H) =
      if (isEmpty(b)) (a, b)
      else drainTo(insert(findMin(b), a), deleteMin(b))

    @tailrec
    def checkSorted(h: H, prev: A): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        if (m < prev) false
        else checkSorted(deleteMin(h), m)
      }

    val (fullA, _) = drainTo(a, b)
    val (fullB, _) = drainTo(b, a)
    val molden = meld(a, b)
    if(isEmpty(a) && isEmpty(b)) true
    else checkSorted(fullA, findMin(fullA)) && checkSorted(fullB, findMin(fullB)) && checkSorted(molden, findMin(molden))
  }

  property("empty2") = forAll { (a: A, b: A) =>
    isEmpty(deleteMin(deleteMin(insert(a, insert(b, empty)))))
  }

  property("minMeld2") = forAll { (a: H, b: H) =>
    if (isEmpty(a) && isEmpty(b)) true
    else if (isEmpty(a)) {
      findMin(meld(a, b)) == findMin(meld(insert(findMin(b), a), deleteMin(b)))
    } else if (isEmpty(b)) {
      findMin(meld(a, b)) == findMin(meld(insert(findMin(a), b), deleteMin(a)))
    } else {
      findMin(meld(a, b)) == findMin(meld(insert(findMin(a), deleteMin(b)), insert(findMin(b), deleteMin(a))))
    }
  }

  property("contains") = forAll { (h: H, a: A) =>
    @tailrec
    def contains(h: H, a: A): Boolean =
      if(isEmpty(h)) false
      else if (findMin(h) == a) true
      else contains(deleteMin(h), a)
      
    contains(insert(a, h), a)
  }
}
