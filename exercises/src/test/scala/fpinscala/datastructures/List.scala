package fpinscala.datastructures

import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {
  import fpinscala.datastructures.{List, Nil}

  "The tail of a non-empty list" should "omit the first value" in {
    assert(List.tail(List(1, 2, 3)) === List(2, 3))
  }

  "The tail of an empty list" should "throw an exception" in {
    assertThrows[RuntimeException] {
      List.tail(Nil)
    }
  }

  "The tail2 of an empty list" should "return None" in {
    assert(List.tail2(Nil) === None)
  }

  "The tail3 of an empty list" should "return an empty list" in {
    assert(List.tail3(Nil) === Nil)
  }

  "Setting the head of a list" should "replaces its first value" in {
    assert(List.setHead(List(1, 2, 3), 0) === List(0, 2, 3))
  }

  "Dropping 2 items of a four item list" should "leave the last two items" in {
    assert(List.drop(List(1, 2, 3, 4), 2) === List(3, 4))
  }

  "Dropping more items than there are in a list" should "return the empty list" in {
    assert(List.drop(List(1, 2), 3) === Nil)
  }

  "Dropping while a predicate holds" should "drop the prefix for which the predicate holds" in {
    assert(List.dropWhile(List(1, 2, 3))(_ <= 2) === List(3))
  }

  "Dropping when the predicate always holds" should "return the empty list" in {
    assert(List.dropWhile(List(1, 2, 3))(_ => true) === Nil)
  }

  "The init of an empty list" should "be an empty list" in {
    assert(List.init(Nil) === Nil)
  }

  "The init of a list" should "include all but the last element" in {
    assert(List.init(List(1, 2, 3)) === List(1, 2))
  }

  "The length of an empty list" should "be 0" in {
    assert(List.length(Nil) === 0)
  }

  "The length of an empty list" should "be equal to its number of elements" in {
    assert(List.length(List(1, 2, 3)) === 3)
  }

  "Folding an empty list leftwise" should "equal the initial value" in {
    assert(List.foldLeft(Nil: List[Int], 0)(_ + _) === 0)
  }

  "Folding a list leftwise" should "combine the values using the passed function" in {
    assert(List.foldLeft(List(1, 2, 3), 0)(_ - _) === -6)
  }
}
