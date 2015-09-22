package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /*
  Exercise 5.1
  Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the REPL.
  You can convert to the regular List type in the standard library.
  You can place this and other functions that operate on a Stream inside the Stream trait.
   */
  def toList: List[A] = foldRight(List[A]())((a, b) => b.::(a))

  /*
  Exercise 5.2
  Write the function take(n) for returning the first n elements of a Stream, and drop(n)
  for skipping the first n elements of a Stream.
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /*
  Exercise 5.3
  Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = sys.error("todo")

  def headOption: Option[A] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
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

  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}

object TestStream {

  import Stream._

  def main(args: Array[String]): Unit = {

    //test toList
    assert((Stream(1, 2, 3, 4, 5).toList) == List(1, 2, 3, 4, 5))

    //test take
    assert(Stream(1, 2, 3, 4, 5).take(3).toList == List(1, 2, 3))

    //test drop
    assert(Stream(1, 2, 3, 4, 5).drop(2).toList == List(3, 4, 5))

    //test takeWhile
    assert(Stream(2, 4, 6, 7, 10).takeWhile((f) => f % 2 == 0).toList == List(2, 4, 6))
  }
}