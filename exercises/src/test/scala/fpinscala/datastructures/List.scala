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
}
