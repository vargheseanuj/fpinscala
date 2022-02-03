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


  def tail[A](l: List[A]): List[A] = l match {
    case Nil=> Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil=> Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => drop(t, n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f)
      else t
    }
  }

  def init[A](l: List[A]): List[A] = {
    def createCons(h: A, con: List[A]):List[A] = con match {
      case Cons(_, Nil) => con
      case Cons(_, t) => createCons(h, t)
    }

    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => createCons(h, t)
    }
  }

  def length[A](l: List[A]): Int = foldRight(l,0)((_,b) => b + 1)

  @scala.annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def lengthFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t: Cons[A]) => foldLeft(t, Cons[A](h, Nil))((b, a) => a match {
      case Nil => b
      case Cons(x: A, _) => Cons[A](x, b)
    })
  }

  def addOneToEach(l: List[Int], updatedList: List[Int] = Nil ): List[Int] = l match {
    case Nil => updatedList
    case Cons(h, t) => addOneToEach(t, Cons(h + 1, t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => map(t)(f)
    }
  }

  def filter[A](l: List[A])(f: A => Boolean):List[A] = {
    def predicate(l: List[A], filteredList: List[A]):List[A] =
      l match {
        case Nil => filteredList
        case Cons(h, t) => if (f(h)) Cons(h, tail(l))
        else filteredList
      }
    l match {
      case Nil => Nil
      case Cons(_, _) => predicate(l, Nil)
    }
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case Nil => Nil
      case Cons(_, _) => flatMap(tail(as))(f)
    }
  }
}
