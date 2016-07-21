package quickcheck

import org.scalacheck.Arbitrary.{apply => _, _}
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    m <- empty
  } yield insert(n, m)
  
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
}
