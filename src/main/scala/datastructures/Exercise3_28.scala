package datastructures

/**
 * Write a function map, analogous to the method of the same name on List,
 * that modifies each element in a tree with a given function.
 */
object Exercise3_28 {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
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
    println(map(tree)(_ * 2))
  }

}
