package fpinscala.datastructures

import org.scalatest.FlatSpec

class TreeSpec extends FlatSpec {
  import fpinscala.datastructures.Tree

  val exampleTree1 = Branch(
    Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Leaf(3)
    ),
    Branch(
      Leaf(4),
      Branch(
        Leaf(5),
        Leaf(6)
      )
    )
  )

  "The size of a tree" should "be the number of leaves and branches in it" in {
    assert(Tree.size(exampleTree1) === 11)
  }

  "The maximum of a tree" should "be the maximum element of all leaves" in {
    assert(Tree.maximum(exampleTree1) === 6)
  }

  "The depth of a tree" should "be the maximum path length from the root to any leaf" in {
    assert(Tree.depth(exampleTree1) === 3)
  }

  "Mapping over a list" should "apply the given functions to the appropriate parts of the tree" in {
    val resultTree = Branch(
      Branch(
        Branch(
          Leaf(2),
          Leaf(3)
        ),
        Leaf(4)
      ),
      Branch(
        Leaf(5),
        Branch(
          Leaf(6),
          Leaf(7)
        )
      )
    )

    assert(Tree.map(exampleTree1)(x => x + 1) === resultTree)
  }
}
