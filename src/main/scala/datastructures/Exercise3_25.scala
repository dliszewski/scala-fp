package datastructures

/**
 * Write a function size that counts the number of nodes (leaves and branches) in a tree.
 */
object Exercise3_25 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def count[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + count(l) + count(r)
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
    println(count(tree))
  }

}
