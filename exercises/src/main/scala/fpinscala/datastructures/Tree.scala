package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A], cnt: Int = 0): Int = t match {
    case Leaf(_) => cnt + 1
    case Branch(l, r) => size(l, cnt) + size(r, cnt)
  }

  def depth[A](value: A, t: Tree[A]): Int = {
    def recurse(th: Tree[A], depth: Int): Int =
      th match {
        case Leaf(v) => if (v == value) depth else -1
        case Branch(l, r) =>
          val left = recurse(l, depth + 1)
          val right = recurse(r, depth + 1)
          if (left > -1) left
          else if (right > -1) right
          else -1
      }

    recurse(t, 0)
  }

  def maximum(t: Tree[Int]): Int = {
    def findMax(th: Tree[Int], max: Int): Int =
      th match {
        case Leaf(v) => max.max(v)
        case Branch(l, r) => findMax(l, max).max(findMax(r, max))
      }
    findMax(t, Int.MinValue)
  }
}