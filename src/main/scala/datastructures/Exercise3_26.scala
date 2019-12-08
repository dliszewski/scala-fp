package datastructures

/**
 * Write a function maximum that returns the maximum element in a Tree[Int].
 * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
 */
object Exercise3_26 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def maxValue(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(l, r) => maxValue(l) max maxValue(r)
  }

  def main(args: Array[String]): Unit = {

    val tree = Branch(
      Branch(
        Branch(Leaf(1), Leaf(2)),
        Branch(Leaf(10), Leaf(4))
      ),
      Branch(
        Branch(Leaf(5), Leaf(6)),
        Branch(Leaf(7), Leaf(8))
      )
    )
    println(maxValue(tree))
  }

}
