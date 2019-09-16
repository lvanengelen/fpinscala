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
}
