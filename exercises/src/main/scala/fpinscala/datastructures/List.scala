package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def head[A](l: List[A]): A = l match {
    case Cons(h, t) => h
    case _ => sys.error("head of empty list")
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => l
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }

  // def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  //   case Nil => l
  //   case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  // }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, x) => x + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Int]): Int =
    foldLeft(ns, 1)(_ * _)

  def length3[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

  def appendViaFoldLeft[A](l: List[A], as: List[A]): List[A] =
    foldLeft(foldLeft(l, List[A]())((acc, h) => Cons(h, acc)), as)((acc, h) => Cons(h, acc))

  def appendViaFoldRight[A](l: List[A], as: List[A]): List[A] =
    foldRight(l, as)(Cons(_, _))

  def flatten[A](ls: List[List[A]]): List[A] =
    foldRight(ls, List[A]())(appendViaFoldRight)

  def penultimate[A](list: List[A]): A =
    foldLeft(list, (head(list), head(list)))((prev, curr) => (prev._2, curr))._1

  def add1(xs: List[Int]): List[Int] =
    foldRight(xs, Nil:List[Int])((h, acc) => Cons(h + 1, acc))

  def doublesToStrings(xs: List[Double]): List[String] =
    foldRight(xs, Nil:List[String])((h, acc) => Cons(h.toString(), acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h, acc) => Cons(f(h), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((h, acc) => f(h) match {
      case true => Cons(h, acc)
      case false => acc
    })

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((h, acc) => append(f(h), acc))

  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = (as1, as2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Cons(h, t) => sub match {
      case Cons(subh, subt) => 
        if (subh == h) {
          if (t == Nil && subt == Nil) true
          else hasSubsequence(t, subt)
        }
        else hasSubsequence(t, sub)
      case _ => true
    }
    case _ => false
  }
}
