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

  "The sum using foldLeft" should "be the same as the normal sum" in {
    assert(List.foldSum(List(1, 2, 3)) === List.sum(List(1, 2, 3)))
  }

  "The product using foldLeft" should "be the same as the normal product" in {
    assert(List.foldProduct(List(1, 2, 3)) === List.product(List(1, 2, 3)))
  }

  "The length using foldLeft" should "be the same as the normal length" in {
    assert(List.foldLength(List(1, 2, 3)) === List.length(List(1, 2, 3)))
  }

  "The reverse of a list" should "contain the elements in backwards order" in {
    assert(List.reverse(List(1, 2, 3)) === List(3, 2, 1))
  }

  "Folding left by folding right" should "give the same result as just folding left" in {
    assert {
      List.foldLeftWithFoldRight(Nil: List[Int], 0)(_ + _) ===
      List.foldLeft(Nil: List[Int], 0)(_ + _)
    }
  }

  "Folding right by folding left" should "give the same result as just folding right" in {
    assert {
      List.foldRightWithFoldLeft(Nil: List[Int], 0)(_ + _) ===
      List.foldRight(Nil: List[Int], 0)(_ + _)
    }
  }

  "Folding right by folding left with reverse" should "give the same result as just folding right" in {
    assert {
      List.foldRightWithFoldLeftAndReverse(Nil: List[Int], 0)(_ + _) ===
      List.foldRight(Nil: List[Int], 0)(_ + _)
    }
  }

  "Appending using foldAppend" should "be the same as the normal append" in {
    assert {
      List.foldAppend(List(1, 2, 3), List(4, 5)) ===
      List.append(List(1, 2, 3), List(4, 5))
    }
  }

  "Concatenating an empty list" should "yield an empty list" in {
    assert(List.concat(Nil) === Nil)
  }

  "Concatenating a list of lists" should "yield a list with the elements of each list" in {
    assert(List.concat(List(List(1, 2), Nil, List(3, 4))) === List(1, 2, 3, 4))
  }

  "Adding one on an integer list" should "increase each element by 1" in {
    assert(List.add1(List(1, 2, 3)) === List(2, 3, 4))
  }

  "Converting a double list to string" should "convert each element to a string" in {
    assert(List.doubleToString(List(1.0, 2.0, 3.0)) === List("1.0", "2.0", "3.0"))
  }

  "Mapping a function over a list" should "yield a list of results of calling the function on each element" in {
    assert(List.map(List(1, 2, 3))(_ + 1) === List(2, 3, 4))
  }

  "Filtering a list" should "yield a list with only the elements for which the predicate holds" in {
    assert(List.filter(List(1, 2, 3, 4, 5))(x => x % 2 == 0) === List(2, 4))
  }

  "Flatmapping a list" should "map the function over the list and concatenate the result" in {
    assert(List.flatMap(List(1, 2, 3))(x => if (x == 2) Nil else List(x, 2 * x)) === List(1, 2, 3, 6))
  }

  "Filtering a list with flatMap" should "yield a list with only the elements for which the predicate holds" in {
    assert(List.filterWithFlatMap(List(1, 2, 3, 4, 5))(x => x % 2 == 0) === List(2, 4))
  }

  "Adding two lists of integers" should "yield a list containing the sum of the elements of both lists" in {
    assert(List.addListsOfInt(List(1, 2, 3), List(4, 5, 6)) === List(5, 7, 9))
  }

  "Adding two lists of integers" should "yield a list with a length of the shortest of both lists" in {
    assert(List.addListsOfInt(List(1, 2, 3, 4, 5), List(4, 5, 6)) == List(5, 7, 9))
  }

  "Zipping two lists" should "apply the given function on matching elements" in {
    assert(List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) === List(5, 7, 9))
  }

  "Zipping two lists" should "hield a list with a length of the shortest of bith lists" in {
    assert(List.zipWith(List(1, 2, 3, 4, 5), List(4, 5, 6))(_ + _) === List(5, 7, 9))
  }
}
