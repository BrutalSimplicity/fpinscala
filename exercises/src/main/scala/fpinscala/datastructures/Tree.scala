package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
	def size[A](t: Tree[A]): Int = t match {
		case Branch(left, right) => 2 + size(left) + size(right)
		case _ => 1
	}

	def maximum(t: Tree[Int]): Int = t match {
		case Branch(left, right) => maximum(left) max maximum(right)
		case Leaf(value) => value
	}

	def depth[A](t: Tree[A]): Int = t match {
		case Branch(left, right) => 1 + (depth(left) max depth(right))
		case _ => 0
	}

	def fold[A, B](t: Tree[A], leaf: A => B)(f: (B, B) => B): B = t match {
		case Branch(left, right) => f(fold(left, leaf)(f), fold(right, leaf)(f))
		case Leaf(value) => leaf(value)
	}

	def size1[A](t: Tree[A]): Int =
		fold[A, Int](t, l => 1)(2 + _ + _)

	def maximum1(t: Tree[Int]): Int =
		fold[Int, Int](t, l => l)(_ max _)

	def depth1[A](t: Tree[A]): Int =
		fold[A, Int](t, l => 0)((a, b) => 1 + (a max b))
}