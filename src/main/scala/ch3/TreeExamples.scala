package ch3

/**
  * Created by hyewon on 2016. 1. 1..
  */

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  //  @annotation.tailrec
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)

  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    //    case Branch(l, r) =>  (depth(l) + 1) max (depth(r) +1)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
    case Leaf(n) => l(n)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))

  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(a => 1)((b1, b2) => b1 + b2 + 1)

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)((b1, b2) => b1 max b2)

  def depth2[A](t: Tree[A]): Int = fold(t)(a => 0)((b1, b2) => 1 + (b1 max b2))

  /*
  Note the type annotation required on the expression `Leaf(f(a))`.
   Without this annotation, we get an error like this:
  type mismatch;
    found   : fpinscala.datastructures.Branch[B]
    required: fpinscala.datastructures.Leaf[B]
       fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                      ^
  This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
  Without the annotation, the result type of the fold gets inferred as `Leaf[B]`
  and it is then expected that the second argument to `fold` will return `Leaf[B]`, w
  hich it doesn't (it returns `Branch[B]`).
  Really, we'd prefer Scala to infer `Tree[B]` as the result type in both cases.
  When working with algebraic data types in Scala,
  it's somewhat common to define helper functions that simply call the corresponding data constructors
   but give the less specific result type:

    def leaf[A](a: A): Tree[A] = Leaf(a)
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
  */
  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))


}


object TreeExamples {

  def main(args: Array[String]) {

    println(Tree.size(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))
    println(Tree.size2(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))

    println(Tree.maximum(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))
    println(Tree.maximum2(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))
    println(Tree.depth(Branch(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))), Leaf(2))))
    println(Tree.depth2(Branch(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))), Leaf(2))))

        val t1: Tree[Int] = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
        println(Tree.map(t1)(_ + 1))
        println(Tree.map2(t1)(_ + 1))
  }
}
