package fpinscala.errorhandling

import org.scalatest.FlatSpec

class EitherSpec extends FlatSpec {
  import fpinscala.errorhandling.Either

  "Flatmapping Right with a function returning Left" should "return Left" in {
    assert(Right(1).flatMap(a => Left(a.toString)) === Left("1"))
  }

  "Flatmapping Left with a function returning Right" should "return the same left value" in {
    assert(Left(1).flatMap(a => Right(a.toString)) === Left(1))
  }

  "Mapping Right" should "return Right with the function applied to the value" in {
    assert(Right(1).map(_.toString) === Right("1"))
  }

  "Mapping Left" should "return the same Left value" in {
    assert(Left(1).map(_.toString) === Left(1))
  }

  "Map2 on two Eithers" should "combine two Right values using a binary function" in {
    assert(Right(1).map2(Right(2))(_ + _) === Right(3))
  }

  "Map2 on two Eithers" should "keep an original Left" in {
    assert((Left(1): Either[Int, Int]).map2(Left(2))(_ + _) === Left(1))
  }

  "Map2 on two Eithers" should "take the passed Left over an original Right" in {
    assert(Right(1).map2(Left(2))(_ + _) === Left(2))
  }

  "Either orElse" should "return the given Either if Left" in {
    assert(Left(1).orElse(Right(2)) === Right(2))
  }

  "Either orElse" should "return the original either if Right" in {
    assert(Right(1).orElse(Right(2)) === Right(1))
  }

  "Either traverse" should "combine a list of values into a Right of a list of values" in {
    assert(Either.traverse(List(1, 2))(Right(_)) === Right(List(1, 2)))
  }

  "Either traverse" should "combine a list of values into a Left" in {
    assert(Either.traverse(List(1, 2, 3, 4))(x => if (x == 2 || x == 3) Left(x.toString) else Right(x)) === Left("2"))
  }

  "Either sequence" should "combine a list of values into a Right of a list of values" in {
    assert(Either.sequence(List(Right(1), Right(2))) === Right(List(1, 2)))
  }

  "Either sequence" should "combine a list of values into a Left" in {
    assert(Either.sequence(List(Right(1), Left("2"), Left("3"), Right(4))) === Left("2"))
  }

  "Either map2 collect" should "combine two Right values using a binary function" in {
    assert(Right(1).map2collect(Right(2))(_ + _) === Right(3))
  }

  "Either map2 collect" should "collect the original value if it is Left" in {
    assert((Left(1): Either[Int, Int]).map2collect(Right(2))(_ + _) === Left(List(1)))
  }

  "Either map2 collect" should "collect the given value if it is Left" in {
    assert(Right(1).map2collect(Left(2))(_ + _) === Left(List(2)))
  }

  "Either map2 collect" should "collect the original and given values if they are both Left" in {
    assert((Left(1): Either[Int, Int]).map2collect(Left(2))(_ + _) === Left(List(1, 2)))
  }
}
