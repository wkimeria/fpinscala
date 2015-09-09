package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /*
  Exercise 3:25
  Write a function size that counts the number of nodes (leaves and branches) in a tree.
  */
  def count[A](tree: Tree[A]): Int = {
    def loop(t: Tree[A], c: Int): Int = t match {
      case Leaf(x) => c + 1
      case Branch(l, r) => 1 + loop(l, c) + loop(r, c)
    }
    loop(tree, 0)
  }

  /*
  Exercise 3:26
  Write a function maximum that returns the maximum element in a Tree[Int]. (Note: In Scala, you can use x.max(y)
  or x max y to compute the maximum of two integers x and y.)
   */
  def max(tree: Tree[Int]): Int = {
    def loop(t: Tree[Int], m: Int): Int = t match {
      case Leaf(x: Int) => x.max(m)
      case Branch(l, r) => loop(l, m).max(loop(r, m))
      case _ => 0
    }
    loop(tree, 0) //Assumes that numbers in list > 0
  }


  /*
  Exercise 3:27
  Write a function depth that returns the maximum path length from the root of a tree to any leaf.
   */
  def depth[A](tree: Tree[A]): Int = {
    def loop[A](t: Tree[A], d: Int): Int = t match {
      case Leaf(x) => d
      case Branch(l, r) => loop(l, d + 1).max(loop(r, d + 1))
    }
    loop(tree, 0)
  }

  /*
  Exercise 3:28
  Write a function map, analogous to the method of the same name on List, that modifies each element in
  a tree with a given function.
   */
  def map[A](t: Tree[A])(f: A => A): Tree[A] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /*
  Exercise 3:29
  Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
  Re-implement them in terms of this more general function.
  Can you draw an analogy between this fold function and the left and right folds for List?
   */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def mapViaFold[A](t: Tree[A])(f: A => A): Tree[A] = {
    fold(t)((x) => Leaf(f(x)): Tree[A])(Branch(_, _))
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(x => 1)(1 + _ + _)
  }

  def maxViaFold(t: Tree[Int]): Int = {
    fold(t)(x => x: Int)(_.max(_))
  }

  def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(x => 0)((a, b) => b + 1)
  }
}

object TestTree {

  import Tree._

  def main(args: Array[String]): Unit = {

    //Test count
    assert(count(Branch(Leaf(1), Leaf(2))) == 3)
    assert(count(Branch(Leaf(9), Branch(Leaf(1), Leaf(2)))) == 5)

    //Test max
    assert(max(Branch(Leaf(1), Leaf(2))) == 2)
    assert(max(Branch(Leaf(9), Branch(Leaf(1), Leaf(2)))) == 9)
    assert(max(Branch(Leaf(11), Branch(Leaf(1), Leaf(2)))) == 11)

    //Test depth
    assert(depth(Branch(Leaf(1), Leaf(2))) == 1)
    assert(depth(Branch(Leaf(9), Branch(Leaf(1), Leaf(2)))) == 2)
    assert(depth(Branch(Leaf(13), Branch(Leaf(5), Branch(Leaf(3), Branch(Leaf(1), Leaf(2)))))) == 4)

    //Test map
    assert(map(Branch(Leaf(1.0), Leaf(2.0)))(x => x * 2) == Branch(Leaf(2.0), Leaf(4.0)))
    assert(map(Branch(Leaf(13), Branch(Leaf(5), Branch(Leaf(3), Branch(Leaf(1), Leaf(2))))))(x => x * 2)
      == Branch(Leaf(26), Branch(Leaf(10), Branch(Leaf(6), Branch(Leaf(2), Leaf(4))))))

    //Test fold
    assert(mapViaFold(Branch(Leaf(9), Branch(Leaf(1), Leaf(2))))(x => x * 2) == Branch(Leaf(18), Branch(Leaf(2), Leaf(4))))
    assert(sizeViaFold(Branch(Leaf(19), Branch(Leaf(10), Leaf(20)))) == 5)
    assert(maxViaFold(Branch(Leaf(11), Branch(Leaf(17), Leaf(2)))) == 17)
    assert(depthViaFold(Branch(Leaf(1), Leaf(2))) == 1)
    assert(depthViaFold(Branch(Leaf(9), Branch(Leaf(1), Leaf(2)))) == 2)
    assert(depthViaFold(Branch(Leaf(13), Branch(Leaf(5), Branch(Leaf(3), Branch(Leaf(1), Leaf(2)))))) == 4)

  }
}