package quickcheck

import org.scalacheck.Arbitrary.{apply => _, _}
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
  } yield insert(n, empty)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMinMelding") = forAll { (h: H) =>
    val h2 = empty
    val minH = findMin(h)
    insert(minH - 1, h2)
    val minMeld = findMin(meld(h, h2))
    minMeld == minH || minMeld == minH - 1

  }

  property("findMinOfBoth") = {

  val v1 = 2
  val v2 = 1
  findMin(insert(v2, insert(v1, empty))) == v2
  }

  property("insert + del = isEmpty") = {


    isEmpty(deleteMin(insert(2, empty)))

  }

  property("Delete in empty") = {

    val r = try {
      deleteMin(empty)
      false
    } catch {
      case e: NoSuchElementException =>
        true
    }

    r
  }

  property("Meld two emtpy") = {

    isEmpty(meld(empty, empty))
  }

  def checkOrder(h: H, last: Int, b: Boolean): Boolean = {

    if (isEmpty(h)) b
    else {
      val currentMin = findMin(h)
      checkOrder(deleteMin(h), currentMin, currentMin > last & b)
    }

  }

  property("Sorted mins") = {

    val h = insert(5, insert(3, insert(-2, empty)))

    checkOrder(deleteMin(h), findMin(h), true)
  }

  property("Meld nil") = {

    val h = insert(5, insert(3, empty))
    val h2 = insert(-2, empty)

    findMin(meld(h, h2))== -2
  }
}
