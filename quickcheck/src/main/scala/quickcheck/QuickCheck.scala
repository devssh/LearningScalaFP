package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- Arbitrary.arbitrary[List[Int]]
  } yield n.foldRight[H](empty)((next: Int, acc: H) => insert(next, acc))
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("check if minimum really works") = forAll {
    (x: A) =>
      findMin(insert(x, empty)) == x
  }

  property("check if minimum really works 2") = forAll {
    (x: A, y: A) =>
      if (x<y) findMin(insert(y, insert(x, empty))) == x
      else findMin(insert(y, insert(x, empty))) == y
  }

  property("check empty") = forAll {
    (x: A) =>
      deleteMin(insert(x, empty)) == empty
  }

  def heapToList(h:H): List[A] = {
    if (isEmpty(h)) List()
    else findMin(h)::heapToList(deleteMin(h))
  }

  property("check length of modified heap tail") = forAll {
    (x:A, h:H) =>
      val oldMin: A = if(isEmpty(h)) x else heapToList(h).min
      val newMin: A = List(x, oldMin).min
      heapToList(deleteMin(insert(x, h))).size == heapToList(h).size
  }

  property("heap contains inserted value") = forAll {
    (x: A, h: H) =>
      heapToList(insert(x, h)) contains x
  }


}
