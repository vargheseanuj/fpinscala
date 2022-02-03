package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A], cnt:Int = 0):Int = t match {
    case Leaf(_) => cnt +1
    case Branch(l, r) => size(l, cnt) + size(r, cnt)
  }

  def depth[A](value: A, t: Tree[A]):Int = ???

  def maximum[A](t: Tree[A]):A = ???



}