package datastructures

object PatternMatching {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    //    exercise 3.2
    def tail[A](ds: List[A]): List[A] = ds match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    //    exercise 3.3
    def setHead[A](ds: List[A], newHead: A): List[A] = ds match {
      case Nil => Cons(newHead, Nil)
      case Cons(_, xs) => Cons(newHead, xs)
    }

    //    exercise 3.4
    def drop[A](list: List[A], n: Int): List[A] = {
      if (n <= 0) {
        return list
      }
      list match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }
    }

    //    exercise 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(head, tail) => if (f(head)) {
          dropWhile(tail, f)
        } else {
          Cons(head, tail)
        }
      }
    }

    //  exercise 3.6
    def init[A](l: List[A]): List[A] = {
      l match {
        case Cons(_, Nil) => Nil
        case Cons(head, t) => Cons(head, init(t))
      }
    }

    def init2[A](l: List[A]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]

      @annotation.tailrec
      def go(cur: List[A]): List[A] = cur match {
        case Nil => sys.error("init of empty list")
        case Cons(_, Nil) => List(buf.toList: _*)
        case Cons(h, t) => buf += h; go(t)
      }

      go(l)
    }

    //3.7 No
    //3.8
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _)

    // 3.9
    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, acc) => acc + 1)
    }

    // 3.10
    @scala.annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }
    }

    // 3.11
    def sumFL(ints: List[Int]): Int = foldLeft(ints, 0)((x, y) => x + y)

    def productFL(ints: List[Int]): Int = foldLeft(ints, 1)((x, y) => x * y)

    def lengthFL[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

    //3.12
    def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

    //3.13
    //SKIP

    //3.14
    def append[A](as: List[A], a: List[A]): List[A] = foldRight(as, a)(Cons(_, _))

    //3.15
    def concat[A](as: List[List[A]]): List[A] = foldRight(as, Nil: List[A])(append)

    //3.16
    def addOne(as: List[Int]): List[Int] = foldRight(as, Nil: List[Int])((h, t) => Cons(h + 1, t))

    //3.17
    def myToString(as: List[Double]): List[String] = foldRight(as, Nil: List[String])((h, t) => Cons(h.toString, t))

    //3.18
    def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

    //3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((h, t) =>
      if (f(h)) {
        Cons(h, t)
      } else {
        t
      }
    )

    //3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

    //3.21
    def filterFM[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

    //3.22
    def add1(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, add1(t1, t2))
    }

    //3.23
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    //3.24
    //SKIP HARD :)


  }

  val ex3: List[String] = Cons("a", Cons("b", Nil))


  def main(args: Array[String]): Unit = {
    //    exercise 3.2
    //    println(List.tail(List()))
    //    println(List.tail(List(1)))
    //    println(List.tail(List(1, 2)))

    //    exercise 3.3
    //    println(List.setHead(List(), 7))
    //    println(List.setHead(List(1), 7))
    //    println(List.setHead(List(1, 2), 7))

    //    exercise 3.4
    //        println(List.drop(List(), 0))
    //        println(List.drop(List(1), 0))
    //        println(List.drop(List(1, 2), 1))
    //        println(List.drop(List(1, 2, 3), 2))
    //        println(List.drop(List(1, 2, 3), 5))

    //    exercise 3.5
    //    println(List.dropWhile(List(), (value: Int) => value < 3))
    //    println(List.dropWhile(List(1), (value: Int) => value < 3))
    //    println(List.dropWhile(List(1, 2, 3, 4, 5), (value: Int) => value < 3))
    //    println(List.dropWhile(List(6), (value: Int) => value < 3));

    //    exercise 3.6
    //    println(List.init(List(1, 2, 3, 4)))
    //    println(List.init2(List(1, 2, 3, 4)))

    //    exercise 3.8
    //    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

    //    exercise 3.9
    //     println (List.length(List(1, 2, 3)))

    //    exercise 3.10
    //    println(List.foldLeft(List(1, 2, 3), 0)((x, y) => x + y))

    //    exercise 3.11
    //    println(List.sumFL(List()))
    //    println(List.productFL(List(1,2,3)))
    //    println(List.lengthFL(List(1,2)))

    //    exercise 3.12
    //    println(List.reverse(List(1, 2, 3)))

    //    exercise 3.13
    //    SKIP

    //    exercise 3.14
    //    println(List.append(List(1, 2, 3), List(6, 7)))

    //    exercise 3.15
    //    println(List.concat(List(List(1, 2, 3), List(4, 5, 6))))

    //    exercise 3.16
    //    println(List.addOne(List(1, 2, 3)))

    //    exercise 3.17
    //    println(List.myToString(List(1d, 2d, 3d)))

    //    exercise 3.18
    //        println(List.map(List(1, 2, 3, 4, 5, 6))(_ * 2))

    //    exercise 3.19
    //    println(List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))

    //    exercise 3.20
    //    println(List.flatMap(List(1, 2, 3))(i => List(i, i)))

    //    exercise 3.21
    //    println(List.filterFM(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))

    //    exercise 3.22
    //    println(List.add1(List(1, 2, 3), List(4, 5, 6)))

    //    exercise 3.23
    println(List.zipWith(List(1, 2, 3), List("a", "b", "c"))((x, y) => x + y))

    //    exercise 3.24
    // SKIP HARD

    //    exercise 3.25

  }

}
