package fpinscala.errorhandling

import org.scalatest.FlatSpec

class OptionSpec extends FlatSpec {
  import fpinscala.errorhandling.Option

  "Mapping Some" should "return Some with the function applied to the value" in {
    assert(Some(1).map(_.toString) === Some("1"))
  }

  "Mapping None" should "return None" in {
    assert(None.map(_.toString) === None)
  }

  "Getting the value of Some" should "return the value" in {
    assert(Some(1).getOrElse(2) === 1)
  }

  "Getting the value of None" should "return the default value" in {
    assert(None.getOrElse(2) === 2)
  }

  "Flatmapping Some with a function returning None" should "return None" in {
    assert(Some(1).flatMap(_ => None) === None)
  }

  "Flatmapping Some with a function returning Some" should "return Some" in {
    assert(Some(1).flatMap(x => Some(x.toString)) === Some("1"))
  }

  "Flatmapping None with a function returning Some" should "return None" in {
    assert(None.flatMap(x => Some(x.toString)) === None)
  }

  "Flatmapping None with a function returning None" should "return None" in {
    assert(None.flatMap(_ => None) === None)
  }

  "Some orElse another Some" should "return the original Some" in {
    assert(Some(1).orElse(Some(2)) === Some(1))
  }

  "None orElse another Some" should "return the other Some" in {
    assert(None.orElse(Some(2)) === Some(2))
  }

  "Filtering Some" should "return the original Some if the predicate holds on the value" in {
    assert(Some(1).filter(_ > 0) === Some(1))
  }

  "Filtering Some" should "return None if the predicate does not hold on the value" in {
    assert(Some(1).filter(_ < 0) === None)
  }

  "Filtering None" should "return None" in {
    assert((None: Option[Int]).filter(_ > 0) === None)
  }

  "The variance of an empty sequence" should "return None" in {
    assert(Option.variance(Seq.empty) === None)
  }

  "The variance of a list" should "return Some(variance)" in {
    assert(Option.variance(Seq(1, 2, 3, 4, 5)) === Some(2.0))
  }

  "Map2 on two options" should "combine two option values using a binary function" in {
    assert(Option.map2(Some(1), Some(2))(_ + _) === Some(3))
  }

  "Map2 with a None" should "return None" in {
    assert(Option.map2(Some(1), None: Option[Int])(_ + _) === None)
    assert(Option.map2(None: Option[Int], Some(1))(_ + _) === None)
  }

  "Sequence" should "combine a list of Some values into a Some of a list of values " in {
    assert(Option.sequence(List(Some(1), Some(2))) === Some(List(1, 2)))
  }

  "Sequence" should "combine a list of Options into None if the list contains any None" in {
    assert(Option.sequence(List(Some(1), None, Some(2))) === None)
  }

  "Traverse" should "combine a list of Some values into a Some of a list of values " in {
    assert(Option.traverse(List(1, 2))(Some(_)) === Some(List(1, 2)))
  }

  "Traverse" should "combine a list of Options into None if the list contains any None" in {
    assert(Option.traverse(List(1, 2, 3))(x => if (x == 2) None else Some(x)) === None)
  }
}
