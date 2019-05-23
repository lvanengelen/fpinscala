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
}
