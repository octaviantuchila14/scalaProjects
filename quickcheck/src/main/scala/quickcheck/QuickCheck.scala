package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  def extractElems(h: H) : List[Int] = isEmpty(h) match {
    case true => List.empty
    case false => findMin(h) :: extractElems(deleteMin(h))
  }

  def insertElems(l: List[Int]) : H = l match {
    case Nil => empty
    case x :: xs => insert(x, insertElems(xs))
  }


  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    (a < b) match {
      case true => (findMin(h) == a)
      case false => (findMin(h) == b)
    }
  }

  property("delMinMakesEmpty") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("deconstructSame") = forAll { (a: Int, b: Int, c: Int, d: Int, e: Int) =>
    val as = List(a, b, c, d, e)
    as.sorted == extractElems(insertElems(as))
  }

  property("sorted") = forAll { (h: H) =>

    val el = extractElems(h)

    println("heap h is: " + h.toString())
    println("extract elems: " + el)
    println("sorted? : " + el.dropRight(1).zip(el.tail).forall(x => x._1 <= x._2))

    el.dropRight(1).zip(el.tail).forall(x => x._1 <= x._2)
  }

  property("minOfTwo") = forAll { (h1: H, h2: H) =>
    val h3 = insertElems(extractElems(h1) ++ extractElems(h2))
    println("minfOfBoth is: " + findMin(h3))
    println("minOfEach is: " + findMin(h1) + " " +  findMin(h2))
    (findMin(h3) == findMin(h1) ||  findMin(h3) == findMin(h2))
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  def add(x: Int, xs: List[Int]): List[Int] = {
    return x :: xs
  }

//  lazy val genArray: Gen[List[Int]] = for {
//    x <- arbitrary[Int]
//    xs <- oneOf(List.empty[Int], genHeap).asInstanceOf[List]
//  } yield add(x, xs)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
