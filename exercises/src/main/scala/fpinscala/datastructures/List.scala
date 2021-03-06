package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightVerbose[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    println("as = " + as  + " z = " + z)
    as match {
      case Nil => {
        //println("matched Nil")
        z
      }
      case Cons(x, xs) => {
        //println("matched Cons(x, xs) x = " + x + " xs = " + xs)
        f(x, foldRightVerbose(xs, z)(f))
      }
    }
  }


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  /*
  Exercise 3.2

  Implement the function tail for removing the first element of a List. Note that the function takes constant time.
  What are different choices you could make in your implementation if the List is Nil?
   */
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, x) => x
    case Nil => Nil
  }

  /*
  Exercise 3.3

  Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
   */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, x) => Cons(h, x)
    case Nil => Nil
  }

  /*
  Exercise 3.4

  Generalize tail to the function drop, which removes the first n elements from a list. Note that this function takes
  time proportional only to the number of elements being dropped—we don’t need to make a copy of the entire List.
   */
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }

  /*
  Exercise 3.5
  Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => {
        if (f(h) == true) dropWhile(t, f)
        else Cons(h, dropWhile(t, f))
      }
    }

  /*
  Exercise 3.6
  Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but
  the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3).
  Why can’t this function be implemented in constant time like tail?
   */
  def init[A](l: List[A]): List[A] = {
    def loop(a: List[A]): List[A] = {
      a match {
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, loop(t))
        case Nil => Nil
      }
    }
    loop(l)
  }

  /*
  * Exercise 3.6
  * Tail recursive version
   */
  def init2[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(b: List[A], acc: List[A]):List[A] = {
      b match {
        case Cons(h, Nil) => acc
        case Cons(h, t) => loop(t, Cons(h,acc))
        case _ => Nil
      }
    }
    reverse(loop(l, List[A]()))
  }

  /*
  Exercise 3.9
  Compute the length of a list using foldRight.
   */
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x, b) => b + 1)
  }

  def lengthRightVerbose[A](l: List[A]): Int = {
    foldRightVerbose(l, 0)((x, b) => b + 1)
  }

  def foldRightDemo[A](l: List[A]): Int = {
    foldRight(l, 0)((x, b) => {
      println("x = " + x + " b = " + b);
      b + 1
    })
  }

  /*
  Exercise 3:10
  Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for
  large lists (we say it’s not stack-safe).
  Convince yourself that this is the case, and then write another general list-recursion function,
  foldLeft, that is tail-recursive, using the techniques we discussed in the previous chapter.
   */
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def foldLeftVerbose[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    println("l = " + l  + " z = " + z)
    l match {
      case Nil => {
        //println("matched Nil")
        z
      }
      case Cons(x, xs) => {
        //println("matched Cons(x, xs) x = " + x + " xs = " + xs)
        foldLeftVerbose(xs, f(z, x))(f)
      }
    }
  }

  /*
  Exercise 3:11
  Write sum, product, and a function to compute the length of a list using foldLeft.
   */
  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)((y, x) => x + y)

  def productLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def lengthLeft[A, B](l: List[A]): Int = {
    foldLeft(l, 0)((b, x) => b + 1)
  }

  def lengthLeftVerbose[A, B](l: List[A]): Int = {
    foldLeftVerbose(l, 0)((b, x) => b + 1)
  }

  def foldLeftDemo[A, B](l: List[A]): Int = {
    foldLeft(l, 0)((b, x) => {
      println("b = " + b + " x = " + x);
      b + 1
    })
  }

  /*
  Exercise 3:12
  Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
  See if you can write it using a fold.
  */
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
  }

  /*
  TODO: Write This
  Exercise 3:13
  Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
  Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively,
  which means it works even for large lists without overflowing the stack.
   */

  /*
  Exercise 3:14
  Implement append in terms of either foldLeft or foldRight.
   */
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight((a1), a2)((h, acc) => Cons(h, acc))
  }

  /*
  Exercise 3:15
  Hard: Write a function that concatenates a list of lists into a single list. Its runtime
  should be linear in the total length of all lists. Try to use functions we have already defined.
   */
  def flattenLists[A](l: List[List[A]]): List[A] = {
    foldRight((l), List[A]())((l1, l2) => appendViaFoldRight(l1, l2))
  }

  /*
  Exercise 3:16
  Write a function that transforms a list of integers by adding 1 to each element.
  (Reminder: this should be a pure function that returns a new List!)
   */
  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
  }

  /*
  Exercise 3:17
  Write a function that turns each value in a List[Double] into a String. You can use the expression
  d.toString to convert some d: Double to a String.
   */
  def doubleListToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  /*
  Exercise 3:18
  Write a function map that generalizes modifying each element in a list while maintaining the structure
  of the list. Here is its signature
   */
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def addOneUsingMap(l: List[Int]): List[Int] = map(l)((x) => x + 1)

  def doubleListToStringUsingMap(l: List[Double]): List[String] = map(l)((x) => x.toString)

  /*
  Exercise 3:19
  Write a function filter that removes elements from a list unless they satisfy a given predicate.
  Use it to remove all odd numbers from a List[Int].

  def filter[A](as: List[A])(f: A => Boolean): List[A]
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Cons(h, t) => {
        if (f(h)) Cons(h, filter(t)(f))
        else filter(t)(f)
      }
      case Nil => Nil
    }
  }

  /*
  Exercise 3:20
  Write a function flatMap that works like map except that the function given will return a list instead of a
  single result, and that list should be inserted into the final resulting list. Here is its signature:

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]

  For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight[A, List[B]](as, Nil: List[B])((h, t) => append(f(h), t))
  }

  /*
  Exercise 3:21
  Use flatMap to implement filter.
   */
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap[A, A](as)((a) => {
      if (f(a)) List(a)
      else Nil
    })
  }

  /*
  Exercise 3:22
  Write a function that accepts two lists and constructs a new list by adding corresponding elements.
  For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
   */
  def append2Lists(l1: List[Int], l2: List[Int]): List[Int] = {
    l1 match {
      case Cons(h1: Int, t1) =>
        l2 match {
          case Cons(h2: Int, t2) => {
            Cons((h1 + h2), append2Lists(t1, t2))
          }
          case Nil => Nil
        }
      case Nil => Nil
    }
  }

  /*
  Exercise 3:23
  Generalize the function you just wrote so that it’s not specific to integers or addition.
  Name your generalized function zipWith.
   */
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    l1 match {
      case Cons(h1, t1) =>
        l2 match {
          case Cons(h2, t2) => {
            Cons(f(h1, h2), zipWith(t1, t2)(f))
          }
          case Nil => Nil
        }
      case Nil => Nil
    }
  }

  /*
  Exercise 3:24
  Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence.
  For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
  You may have some difficulty finding a concise purely functional implementation that is also efficient.
  That’s okay. Implement the function however comes most naturally.
  We’ll return to this implementation in chapter 5 and hopefully improve on it.
  Note: Any two values x and y can be compared for equality in Scala using the expression x == y.

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean
   */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def loop(a: List[A], b: List[A], prevMatch: Boolean): Boolean = {
      (a, b) match {
        case ((Cons(h1, t1), Cons(h2, t2))) => {
          if (h1 == h2) loop(t1, t2, true)
          else loop(t1, b, false)
        }
        case (Cons(_, _), Nil) => true
        case (_, _) => false
      }
    }
    (sup, sub) match {
      case (Cons(h1, Nil), Cons(h2, Nil)) => (h1 == h2)
      case (_, _) => loop(sup, sub, false)
    }
  }
}

object TestList {

  import List._

  def main(args: Array[String]): Unit = {
    //Test tail
    assert(tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
    assert(tail(List(1)) == Nil)
    assert(tail(Nil) == Nil)

    //Test Head
    assert(setHead(List(1, 2, 3, 4, 5), 99) == List(99, 2, 3, 4, 5))

    //Test drop
    assert(drop(List(1, 2, 3, 4, 5), 1) == List(2, 3, 4, 5))
    assert(drop(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5))
    assert(drop(List(1, 2, 3, 4, 5), 3) == List(4, 5))

    //Test dropWhile
    assert(dropWhile(List(1, 2, 3, 4, 5, 6), ((x: Int) => x % 2 == 0)) == List(1, 3, 5))

    //Test init
    assert(init(List(1)) == Nil)
    assert(init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))

    //Test init2
    assert(init2(List(1)) == Nil)
    assert(init2(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))

    //Test length using foldRight
    assert(length(Nil) == 0)
    assert(length(List(1)) == 1)
    assert(length(List(1, 2, 3, 4, 66)) == 5)

    //Test using sumLeft
    assert(sumLeft(List(1, 2, 3, 4, 5)) == 15)

    //Test using productLeft
    assert(productLeft(List(1, 2, 3, 4)) == 24)

    //Test length using foldLeft
    assert(lengthLeft(Nil) == 0)
    assert(lengthLeft(List(1)) == 1)
    assert(lengthLeft(List(1, 2, 3, 4, 66)) == 5)

    //Illustrating the differences between foldRight and foldLeft
    /*
    val lst = List(1, 2, 3, 4)
    println("foldRightDemo ----------------------- " + lst)
    foldRightDemo(lst)
    println("foldLeftDemo ----------------------- " + lst)
    foldLeftDemo(lst)
    */

    //Test reverse
    assert(reverse(List(2, 4, 6, 10)) == List(10, 6, 4, 2))

    //Test
    assert(appendViaFoldRight(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))

    //Test Flatten Lists
    assert(flattenLists(List(List(1, 2), List(3, 4), List(5))) == List(1, 2, 3, 4, 5))

    //Test addOne
    assert(addOne(List(1, 2, 3, 4)) == List(2, 3, 4, 5))

    //Test doubleListToString
    assert(doubleListToString(List(1, 2, 3, 4)) == List("1.0", "2.0", "3.0", "4.0"))

    //Test addOneUsingMap
    assert(addOneUsingMap(List(1, 2, 3, 4)) == List(2, 3, 4, 5))

    //Test doubleListToStringUsingMap
    assert(doubleListToStringUsingMap(List(1, 2, 3, 4)) == List("1.0", "2.0", "3.0", "4.0"))

    //Test Filter
    assert(filter(List(1, 2, 3, 4, 5, 6))((x) => x % 2 == 0) == List(2, 4, 6))

    //Test FlatMap
    assert(flatMap(List(1, 2, 3))((a) => List(a, a)) == List(1, 1, 2, 2, 3, 3))

    //Test filterUsingFlatMap
    assert(filterUsingFlatMap(List(1, 2, 3, 4, 5, 6))((x) => x % 2 == 0) == List(2, 4, 6))

    //Test append2Lists
    assert(append2Lists(List(1, 2, 3, 4), List(10, 20, 30, 40)) == List(11, 22, 33, 44))

    //Test zipWith
    assert(zipWith[Int](List(1, 2, 3, 4), List(10, 20, 30, 40))((x, y) => x + y) == List(11, 22, 33, 44))

    //Test hasSubsequence
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3)) == true)
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)) == true)
    assert(hasSubsequence(List(1, 2, 3, 4), List(5, 6, 7)) == false)
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2, 4)) == false)
    assert(hasSubsequence(List(1, 1, 3, 4), List(1, 1, 3)) == true)
    assert(hasSubsequence(List(1, 1, 3, 3), List(1, 1, 4)) == false)
    assert(hasSubsequence(List(1, 2), List(1)) == true)
    assert(hasSubsequence(List(1), List(1)) == true)

    println("----------------- right --------------------------")
    lengthRightVerbose(List(1, 2, 3, 4))
    println("----------------- left --------------------------")
    lengthLeftVerbose(List(1, 2, 3, 4))

  }
}
