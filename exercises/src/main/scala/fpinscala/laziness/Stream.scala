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

  /*
  Exercise 5.4
  Implement forAll, which checks that all elements in the Stream match a given predicate.
  Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
   */
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  /*
  Exercise 5.5
  Use foldRight to implement takeWhile.
   */
  def takeWhileUsingRightFold(p: A => Boolean): Stream[A] = {
    this.foldRight(Stream[A]())((a, b) => p(a) match {
      case true => cons(a, b)
      case false => empty
    })
  }

  /*
   Exercise 5.6
   Hard: Implement headOption using foldRight.
   */

  def headOption: Option[A] =
    this.foldRight(None: Option[A])((a, b) => Some(a))


  /*
  Exercise 5.7
  Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument.
   Part of the exercise is writing your own function signatures.
   */
  //foldRight(empty[B])((h,t) => cons(f(h), t))
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => {
      cons(f(h), t)
    })
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => {
      f(h) match {
        case true => cons(h, t)
        case _ => t
      }
    })
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }


  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }

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


  /*
  Exercise 5.8
  Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
   */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /*
  Exercise 5.9
  Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.[7]
   */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /*
  Exercise 5.10
  Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
   */
  def fib: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = cons(a, loop(b, a + b))
    loop(0, 1)
  }

  /*
  Exercise 5.11
  Write a more general stream-building function called unfold. It takes an initial state, and a
  function for producing both the next state and the next value in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a: A, s: S)) => cons(a,unfold(s)(f))
      case None => Stream[A]()
    }
  }

  /*
  Exercise 5:12
  Write fibs, from, constant, and ones in terms of unfold.[8]
   */
  def fibUsingUnfold[A]: Stream[Int] = {
    unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }
  }

  def fromUsingUnfold(n: Int): Stream[Int] = {
    unfold(n)(x => Some(x,x+1))
  }
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

    //test forAll
    assert(Stream(2, 4, 9, 7, 10).forAll((f) => f % 2 == 0) == false)

    //test takeWhileUsingRightFold
    assert(Stream(2, 4, 6, 7, 10).takeWhileUsingRightFold((f) => f % 2 == 0).toList == List(2, 4, 6))

    //test headOption
    assert(Stream(2, 4, 6, 7, 10).headOption == Some(2))

    //test map
    assert(Stream(1, 2, 3).map(f => f * f).toList == List(1, 4, 9))

    //test filter
    assert(Stream(1, 2, 3, 4, 5, 6, 7).filter(f => f % 2 == 0).toList == List(2, 4, 6))

    //test append
    assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))

    //test flatMap
    assert(Stream(1, 2, 3).flatMap(f => Stream(0, f)).toList == List(0, 1, 0, 2, 0, 3))

    //test constant
    assert(constant(99).take(3).toList == List(99, 99, 99))

    //test from
    assert(from(1).take(3).toList == List(1, 2, 3))
    assert(from(10).take(3).toList == List(10, 11, 12))

    //test assert
    assert(fib.take(8).toList == List(0, 1, 1, 2, 3, 5, 8, 13))

    //test fibUsingUnfold
    assert(fibUsingUnfold.take(8).toList == List(0, 1, 1, 2, 3, 5, 8, 13))

    //test fromUsingUnfold
    assert(fromUsingUnfold(5).take(5).toList == List(5, 6, 7, 8, 9))

  }
}