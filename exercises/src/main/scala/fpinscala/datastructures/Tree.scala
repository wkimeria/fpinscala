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

  /*
  Exercise 3:28
  Write a function map, analogous to the method of the same name on List, that modifies each element in
  a tree with a given function.
   */

  /*
  Exercise 3:29
  Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
  Reimplement them in terms of this more general function.
  Can you draw an analogy between this fold function and the left and right folds for List?
   */
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
  }
}