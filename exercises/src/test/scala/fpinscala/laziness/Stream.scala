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

  "Unfold" should "produce values while the stream-building function returns Some" in {
    assert(Stream.unfold(0)(s => Some(s.toString, s + 1)).take(3).toList === List("0", "1", "2"))
  }

  "Unfold" should "terminate the Stream when the stream-building function returns None" in {
    assert(Stream.unfold(0)(s => if (s != 3) Some(s.toString, s + 1) else None).toList === List("0", "1", "2"))
  }

  "ZipWith" should "result in a Stream with the length of the shortest Stream" in {
    assert(Stream(1, 2, 3).zipWith(Stream("a", "b"))((x, y) => x.toString + y).toList === List("1a", "2b"))
  }

  "ZipWith" should "work properly with an unlimited Stream and a limited Stream" in {
    assert(Stream.ones.zipWith(Stream("a", "b"))((x, y) => x.toString + y).toList === List("1a", "1b"))
  }

  "ZipWith" should "work properly with two unlimited Streams" in {
    assert(Stream.ones.zipWith(Stream.ones)(_ + _).take(3).toList === List(2, 2, 2))
  }

  "ZipAll" should "result in a Stream of pairs of Options" in {
    assert(Stream(1, 2, 3, 4).zipAll(Stream("a", "b")).toList ===
      List((Some(1), Some("a")), (Some(2), Some("b")), (Some(3), None), (Some(4), None)))
  }

  "ZipAll" should "work properly with an unlimited Stream and a limited Stream" in {
    assert(Stream.ones.zipAll(Stream("a", "b")).take(4).toList ===
      List((Some(1), Some("a")), (Some(1), Some("b")), (Some(1), None), (Some(1), None)))
  }

  "ZipAll" should "work properly with two unlimited Streams" in {
    assert(Stream.ones.zipAll(Stream.ones).take(4).toList ===
      List((Some(1), Some(1)), (Some(1), Some(1)), (Some(1), Some(1)), (Some(1), Some(1))))
  }

  "StartsWith" should "return true when the first stream starts with all elements of the second stream" in {
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
  }

  "StartsWith" should "return false when the first stream does not start with all elements of the second stream" in {
    assert(!Stream(1, 2, 3).startsWith(Stream(2, 3)))
  }

  "StartsWith" should "handle an empty first stream correctly" in {
    assert(!Stream.empty[Int].startsWith(Stream(1, 2)))
  }

  "StartsWith" should "handle an empty second stream corectly" in {
    assert(Stream(1, 2, 3).startsWith(Stream.empty[Int]))
  }
}
