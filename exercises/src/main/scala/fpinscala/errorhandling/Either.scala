package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _}

// hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {

  /*
  Exercise 4.6
  Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(b) => Left(b)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(b) => Left(b)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(x) => Right(x)
      case Left(v) => b
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield (f(aa, bb))
  }
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sys.error("todo")

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = sys.error("todo")

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}

object TestEither {

  import Either._

  def main(args: Array[String]): Unit = {

    //Test map
    assert(Right(3).map((f: Int) => f * f) == Right(9))
    assert(Left(3).map((f: Int) => f * f) == Left(3))

    //Test flatMap
    assert(Right(3).flatMap((f: Int) => Right(f * f)) == Right(9))
    assert(Left(3).flatMap((f: Int) => Right(f * f)) == Left(3))

    //Test orElse
    assert(Right(3).orElse(Right(10)) == Right(3))
    assert(Left(3).orElse(Right(10)) == Right(10))
    assert(Left(3).orElse(Left(10)) == Left(10))

    assert(Right(3).map2[Int, Int, Int](Right(6))(_ + _) == Right(9))
    assert(Right(3).map2[Int, Int, Int](Left(6))((a, b) => a + b) == Left(6))
    
  }
}