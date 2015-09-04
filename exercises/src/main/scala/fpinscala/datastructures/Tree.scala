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
      case Branch(l,r) => 1 + loop(l,c) + loop(r, c)
    }
    loop(tree, 0)
  }
}

object TestTree {

  import Tree._

  def main(args: Array[String]): Unit = {

    //Test count
    assert(count(Branch(Leaf(1),Leaf(2))) == 3)
    assert(count(Branch(Leaf(9),Branch(Leaf(1),Leaf(2)))) == 5)

  }
}