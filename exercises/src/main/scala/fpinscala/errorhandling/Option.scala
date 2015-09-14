package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _}
import Math._

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  /*
  Exercise 4.1
  Implement all of the preceding functions on Option. As you implement each function, try to think about what it
  means and in what situations you’d use it. We’ll explore when to use each of these functions next.
  Here are a few hints for solving this exercise:

  It’s fine to use pattern matching, though you should be able to implement all the functions besides map and
  getOrElse without resorting to pattern matching.

  For map and flatMap, the type signature should be enough to determine the implementation.

  getOrElse returns the result inside the Some case of the Option, or if the Option is None,
  returns the given default value.

  orElse returns the first Option if it’s defined; otherwise, it returns the second Option.

   */
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /*
  Exercise 4:2
  Implement the variance function in terms of flatMap. If the mean of a sequence is m,
  the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  See the definition of variance on Wikipedia (http://mng.bz/0Qsr).

  def variance(xs: Seq[Double]): Option[Double]
   */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap((m) => {
      mean(xs.map(x => Math.pow(x - m, 2)))
    })

  /*
  Exercise 4.3
  Write a generic function map2 that combines two Option values using a binary function.
  If either Option value is None, then the return value is too. Here is its signature:

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (a1 => b map (b1 => f(a1, b1)))

  /*
  //TODO: Don't fully understand what is happening here
  Exercise 4.4
  Write a function sequence that combines a list of Options into one Option containing a list of all
  the Some values in the original list.
  If the original list contains None even once, the result of the function should be None;
  otherwise the result should be Some with a list of all the values.
   */

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  /*
  TODO: Revisit this
  Exercise 4.5
  Implement this function. It’s straightforward to do using map and sequence, but try for a more
  efficient implementation that only looks at the list once.
  In fact, implement sequence in terms of traverse.
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => {
        (traverse(t)(f)) match {
          case Some(v: B) => Some(v.::(f(h) match {
            case Some(x) => x
          }))
          case None => None
        }
      }
    }
  }
}

object TestOption {

  import Option._

  def main(args: Array[String]): Unit = {

    //test map
    assert(Some[Int](3).map((a) => a) == Some(3))
    assert(None.map((a) => a) == None)

    //test getOrElse
    assert(Some(3).getOrElse(-1) == 3)
    assert(None.getOrElse(-1) == -1)

    //test flatMap
    assert(Some[Int](3).flatMap((a) => Some(a)) == Some(3))
    assert(None.flatMap((a) => Some(a)) == None)

    //test variance
    assert(variance(List(2.0, 2.0, 5.0, 7.0)) == Some(4.5))

    //test map2
    assert(map2(Some(1), Some(2))((a, b) => a + b) == Some(3))
    assert(map2(None, Some(2))((a: Int, b: Int) => a + b) == None)
    assert(map2(Some(1), None)((a: Int, b: Int) => a + b) == None)
    assert(map2(None, None)((a: Int, b: Int) => a + b) == None)

    //Test sequence
    assert(sequence(List(Some(1), Some(2), None, Some(4))) == None)
    assert(sequence(List(Some(1), Some(2), Some(3), Some(4))) == Some(List(1, 2, 3, 4)))

    //Test traverse
    assert(traverse[Int, String](List(1, 2, 3))((f) => Some(f.toString + "--")) == Some(List("1--", "2--", "3--")))

  }
}