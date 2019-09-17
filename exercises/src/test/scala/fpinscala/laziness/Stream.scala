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

  "For all on an empty Stream" should "result in true" in {
    assert(Stream.empty[Int].forAll(_ => false))
  }

  "For all" should "result in true if the predicate holds for every element" in {
    assert(Stream(1, 2, 3).forAll(_ < 4))
  }

  "For all" should "result in false if the predicate does not hold for some element" in {
    assert(!Stream(1, 2, 3).forAll(_ < 3))
  }

  "Head option" should "result in None for an empty Stream" in {
    assert(Stream.empty[Int].headOption === None)
  }

  "Head option" should "result in Some of the first item of a non-empty Stream" in {
    assert(Stream(1, 2).headOption === Some(1))
  }

  "Mapping a non-empty Stream" should "transform the members" in {
    assert(Stream(1, 2, 3).map(_.toString).toList === List("1", "2", "3"))
  }

  "Filtering a Stream" should "only keep the items matching the predicate" in {
    assert(Stream(1, 2, 3).filter(_ != 2).toList === List(1, 3))
  }

  "Appending a Stream" should "result in the concatenation of the Streams" in {
    assert(Stream(1, 2).append(Stream(3, 4)).toList === List(1, 2, 3, 4))
  }

  "Flatmapping an empty Stream" should "result in an empty Stream of the correct type" in {
    assert(Stream.empty[Int].flatMap(x => Stream("[", x, "]")).toList === List.empty[String])
  }

  "Flatmapping a Stream" should "transform the members and flatten the result" in {
    assert(Stream(1, 2, 3).flatMap(x => Stream("[", x.toString, "]")).toList ===
      List("[", "1", "]", "[", "2", "]", "[", "3", "]"))
  }

  "Constant" should "return an infinite Stream of a given value" in {
    assert(Stream.constant("a").take(3).toList === List("a", "a", "a"))
  }

  "From" should "return an infinite Stream of integers starting at the given value" in {
    assert(Stream.from(42).take(3).toList === List(42, 43, 44))
  }

  "Fibs" should "return an infinite Stream of Fibonacci numbers" in {
    assert(Stream.fibs.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
  }
}
