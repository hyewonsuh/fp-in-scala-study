package ch3

/**
  * Created by hyewon on 2015. 12. 27..
  */


//
//sealed  : 이 trait에 대한 구현이 반드시 이 파일안에 선언되어 있어야 함
// +A : + 는 A가 List의 covariant(공변)매개변수 임을 뜻함
sealed trait List[+A]

//two data constructor  of List
// Nothing은 모든 타입의 subtype임..그러므로 Nil은 모든 List[A]의 하위 타입임 (공변에 대한 의도)
// case object 이므로 그냥 Nil
case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

//재귀적 정의 생성
// => singly linked list
//List("a", "b")
//  ==>  Cons("a", Cons("b", Nil))


//List companion object
object List {


  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Cons(_, xs) => drop(xs, n - 1)
      case Nil => Nil
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (!f(x)) => dropWhile(xs)(f)
    case _ => l
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => Cons(head, xs)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sumByFoldRight(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def productByFoldRight(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def lengthByFoldRight[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => y + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumByFoldLeft(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)


  def productByFoldLeft(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)

  def lengthByFoldLeft(as: List[Int]): Int =
    foldLeft(as, 0)((b, a) => b + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc, h) => Cons(h, acc))


  def foldLeftViaFoldRight[A, B](as: List[A], acc: B)(f: (B, A) => B): B = {

    foldRight(as, acc)((h, acc) => f(acc, h))

  }

  def foldRightViaFoldLeft[A, B](as: List[A], acc: B)(f: (A, B) => B): B = {
    foldLeft(as, acc)((acc, h) => f(h, acc))
  }


  //List(1,2,3)  as is 가변인수 (Seq[A] 타입이 됨)
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // _* 은 Seq를 가변인수로 전달


  //a1의 길이만큼 값복사 , 실행반복
  //append(List(1,2,3), List(4,5,6))
  // ==>  Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))

  }

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  //    foldRight(a1, a2)((ha1, a2) => Cons(ha1, a2))


  //unzip(List(List(1,2,3),List(4,5,6)))
  def unzip(as: List[List[Int]]): List[Int] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(x, unzip(xs))
  }

  def concat(as: List[List[Int]]): List[Int] = foldRight(as, Nil: List[Int])(append)

  def addOne(as: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  def addOne2(as: List[Int]): List[Int] = foldRight(as, Nil: List[Int])((x, y) => Cons(x + 1, y))

  def doubleToString(as: List[Double]): List[String] = foldRight(as, Nil: List[String])((d, s) => Cons(d.toString, s))

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  //  def map2[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])( (a,b) => Cons(f(a), b))
  def map2[A, B](as: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(as, Nil: List[B])((a, b) => Cons(f(a), b))

  def map3[A, B](as: List[A])(f: A => B): List[B] = foldLeft(as, Nil: List[B])((b, a) => Cons(f(a), b))


  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else xs
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil: List[A])((a, z) => if (f(a)) Cons(a, z) else z)

  // List(1,2,3,4) -> List(1,2,3)
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  //flatMap(List(1,2,3)) (i => List(i,i))  = List(1,1,2,2,3,3)
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, b) => append(f(a), b))


  def filterViaFlatMap[A](as: List[A])(f: A => Boolean) = {

    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def addPairwise(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))

  }


  def zipWith[A,B,C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons( f(x,y), zipWith(xs, ys)(f))

  }

  @annotation.tailrec
  def startsWith[A](as: List[A], prefix: List[A]):Boolean = (as, prefix) match {
    case (_, Nil) => true
    case (Cons(h,t),Cons(h2, t2)) if (h==h2) => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSequence[A](sup:List[A], sub:List[A]):Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(x, xs) => hasSequence(xs, sub)
  }
}

import List._

object ListExample {


  def main(args: Array[String]) {

    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b", Nil))

    //    println(tail(ex1))
    //    println(tail(ex2))
    //    println(tail(ex3))
    //
    //    println(setHead(ex1, 1.0))
    //    println(setHead(ex2, 10))
    //    println(setHead(ex3, 10))
    //
    //    println(drop(List(1, 2, 3), 2))
    //    println(drop(List(1, 2, 3, 4), 3))
    //    println(dropWhile(List(1, 2, 3, 4))(_ == 3))
    //
    //    println(append(List(1, 2, 3), List(4, 5, 6)))
    //
    //    println(init(List(1, 2, 3)))
    //    println(init(List(1, 2)))
    //    println(init(List(1)))

    //    println(sumByFoldRight(List(1, 2, 3)))
    //    println(productByFoldRight(List(3, 4, 0, 1.2)))
    //    println(lengthByFoldRight(List(2, 3, 4, 5, 6)))
    //
    //    //    println(foldLeft(List(1,2,3), 1)(_*_))
    //
    //    println(sumByFoldLeft(List(1, 2, 3)))
    //    println(productByFoldLeft(List(3, 4, 0, 1.2)))
    //    println(lengthByFoldLeft(List(2, 3, 4, 5, 6)))
    //
    //
    //    println(reverse(List(1, 2, 3)))
    //
    //    println(reverse(List(1)))

    //    println(foldLeftViaFoldRight(List(1, 2, 3), 0)(_ + _))
    //    println(foldRightViaFoldLeft(List(1, 2, 3), 0)(_ + _))
    //
    //    println(appendViaFoldLeft(List(1, 2, 3), List(4, 5, 6)))
    //
    //    println(unzip(List(List(1, 2, 3), List(4, 5, 6))))
    //    println(concat(List(List(1, 2, 3), List(4, 5, 6))))
    //
    //    println(addOne(List(1, 2, 3)))
    //    println(addOne2(List(1, 2, 3)))
    //
    //    println(doubleToString(List(1.0, 2.0)))
    //    println(map(List(1.0, 2.0))(_.toString))
    //    println(map2(List(1.0, 2.0))(_ + 10))
    //    println(map3(List(1.0, 2.0))(_ + 10))
    //    println(filter(List(1, 2, 3))(_ < 3))
    //    println(filter2(List(1, 2, 3))(_ < 3))

    println(flatMap(List(1, 2, 3))(i => List(i, i)))
    println(flatMap2(List(1, 2, 3))(i => List(i, i)))
    println(flatMap(List(1, 2, 3))(i => List(i)))
    println(filterViaFlatMap(List(1, 2, 3))(i => i > 1))

    println(addPairwise(List(1, 2, 3), List(1, 2, 3)))
    println(zipWith(List(1, 2, 3), List(1, 2, 3))(_+_))

    println(hasSequence(List(1, 2, 3, 4), List(2, 1)))

  }
}

