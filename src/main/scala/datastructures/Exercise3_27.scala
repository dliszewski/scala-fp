package datastructures

/**
 * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
 */
object Exercise3_27 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def depth(tree: Tree[Int]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def main(args: Array[String]): Unit = {

    val tree = Branch(
      Branch(
        Branch(Leaf(1), Leaf(2)),
        Branch(Leaf(3), Leaf(4))
      ),
      Branch(
        Branch(Leaf(5), Leaf(6)),
        Branch(Leaf(7), Leaf(8))
      )
    )
    println(depth(tree))

    val tree2 = Branch(
      Branch(
        Branch(Leaf(1), Leaf(2)),
        Branch(Leaf(3), Leaf(4))
      ),
      Branch(
        Branch(Leaf(5), Leaf(6)),
        Branch(
          Branch(Leaf(7), Leaf(8)),
          Branch(Leaf(9), Leaf(10))
        )
      )
    )
    println(depth(tree2))
  }

}
