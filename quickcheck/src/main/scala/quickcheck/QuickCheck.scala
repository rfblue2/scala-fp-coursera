package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("just two elements") = forAll { (a: Int, b: Int) =>
    val newH = insert(a, insert(b, empty))
    findMin(newH) == math.min(a, b)
  }

  property("insert, min") = forAll { (h: H, a: Int) =>
    val newH = insert(a, h)
    val m = if (isEmpty(h)) a else findMin(h)
    findMin(newH) == math.min(a, m)
  }

  property("delmin empty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("min of meld") = forAll { (h: H, g: H) =>
    if (isEmpty(h) || isEmpty(g)) true
    else findMin(meld(h, g)) == math.min(findMin(h), findMin(g))
  }

  property("insert remove sorted") = forAll { (l : List[Int]) =>
    val h = l.foldLeft(empty)((heap: H, x: Int) => insert(x, heap))
    def getMinList(h: H, mins: List[Int]): List[Int] =
      if (isEmpty(h)) mins else findMin(h) :: mins

    def isSorted(l : List[Int]): Boolean = l match {
      case List() => true
      case _ :: List() => true
      case x :: xs => x < xs.head && isSorted(xs)
    }

    isSorted(getMinList(h, List()))
  }

  property("deleted are ordered") = forAll {
    (f: H) => {
      def getMinList(h: H, mins: List[Int]): List[Int] = {
        if (isEmpty(h)) mins
        else {
          val min = findMin(h)
          getMinList(deleteMin(h), min :: mins)
        }
      }

      def isSorted(l : List[Int]): Boolean = l match {
        case List() => true
        case _ :: List() => true
        case x :: xs => x < xs.head && isSorted(xs)
      }

      isSorted(getMinList(f, List()).reverse)
    }
  }

}
