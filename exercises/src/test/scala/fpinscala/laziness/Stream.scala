package fpinscala.laziness

import org.scalatest.FlatSpec

class StreamSpec extends FlatSpec {
  import fpinscala.laziness.Stream

  "Converting an empty Stream to a List" should "result in an empty list" in {
    assert(Stream.empty[Int].toList === List.empty[Int])
  }

  "Converting a populated Stream to a List" should "result in a populated list" in {
    assert(Stream(1, 2, 3).toList === List(1, 2, 3))
  }

  "Taking items of an empty Stream" should "result in an empty Stream" in {
    assert(Stream.empty[Int].take(2).toList === List.empty[Int])
  }

  "Taking items of a Stream" should "result in a Stream of the given size if there are enough elements" in {
    assert(Stream(1, 2, 3).take(2).toList === List(1, 2))
  }

  "Dropping items from an empty Stream" should "result in an empty Stream" in {
    assert(Stream.empty[Int].drop(2).toList === List.empty[Int])
  }

  "Dropping items from a Stream" should "result in a Stream of the remaining items" in {
    assert(Stream(1, 2, 3, 4).drop(2).toList === List(3, 4))
  }

  "Taking while predicate matches" should "result in a Stream with starting elements matching the predicate" in {
    assert(Stream(1, 2, 3, 2).takeWhile(_ <= 2).toList === List(1, 2))
  }

}
