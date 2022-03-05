package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_,_) if(n < 0) => Empty
    case Cons(h,_) if (n == 1)  => cons(h(), empty)
    case Cons(h, t) => cons(h(), t().take(n-1))

  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n-1)
    case Cons(_, t) if n == 1 => t()
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Empty => empty
  }

  def takeWhile_1(p: A => Boolean): Stream[A] = this.foldRight(empty)((a,b) => if(p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) =>  cons(h(), t()) .forAll(p)
    case _ => false
  }

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _ => None
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B):Stream[B] = this.foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f:A => Boolean):Stream[A] = this.foldRight(empty)((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](v: => Stream[B]):Stream[B] = this.foldRight(v)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]):Stream[B] = this.foldRight(empty[B])((a, b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs(a: Int = 0, b: Int = 1): Stream[Int] = cons(a, fibs(b, a+b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,b)) => cons(a, unfold(b)(f))
    case None => empty
  }

  def fromViaUnfold(x: Int): Stream[Int] = unfold(x)(a => Some(a, a+1))

  def constantViaUnfold(x: Int): Stream[Int] = unfold(x)(a => Some(a, a))

  def fibsViaUnfold: Stream[Int] = {
    val startWith = (0, 1)
    unfold(startWith)(a => Some(a._1, (a._2, a._1 + a._2)))
  }

  def map()


}