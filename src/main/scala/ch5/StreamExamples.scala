package ch5

/**
  * Created by hyewon on 2016. 1. 3..
  */

import Stream._

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h()) // h()를 이용해서 Thunk h를 명시적으로 평가를 강제한다.
  }


  def toList: List[A] = {
    @annotation.tailrec
    def go[A](s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  //    this match {  //not tail recursive
  //    case Empty => Nil
  //    case Cons(h, t) => h() :: t().toList
  //  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)


  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](a: => Stream[B]): Stream[B] =
    foldRight(a)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption //발견즉시 종료 (lazy)

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))


  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) ->(t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def hasSequence[A](sub: Stream[A]): Boolean = {
    tails existsViaFoldRight (_ startsWith sub)
  }

  //Stream(1,2,3) => Stream(Stream(1,2,3), Stream(2,3), Stream(1))
  def tails: Stream[Stream[A]] = {
//    unfold(this) {
//      case Cons(h, t) => Some((cons(h(), t()), t()))
//      case Empty => None
//    }
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)
  }

  //  def startsWithRecursive[A](prefix:Stream[A]):Boolean = (this, prefix) match {
  //    case (Empty, Empty) => false
  //    case (_, Empty) => true
  //    case (Cons(h, t), Cons(ph, pt)) if (h() == ph()) => t().startsWithRecursive(pt())
  //    case _ => false
  //  }

  def startsWith[A](prefix: Stream[A]): Boolean =
    this zipAll prefix takeWhileViaUnfold (!_._2.isEmpty) forAll { case (h, h2) => h == h2 }

  //  scala> Stream(1,2,3).scanRight(0)(_ + _).toList
  //  res2: List[Int] = List(6, 5, 3, 0)
//The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right.
  // It can be implemented using `foldRight` though.
  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight( (z, Stream(z)))( (a, b) => {
      lazy val b2 = b
      val z1: B = f(a, b2._1)
      (z1 , cons(z1, b2._2))
    })._2
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
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  //infinite Stream
  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    // cons(a, constant(a))
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  //n, n+1, n+2 ......
  //The example function `ones` is recursive, how could you define `from`recursively?
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  //0, 1, 1, 2, 3,5, 8...
  def fibs(): Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }
    go(0, 1)
    //    def go(pre: Int, curr: Int): Stream[Int] = {
    //      cons(pre + curr, go(curr, pre + curr))
    //    }
    //    cons(0, cons(1, go(0, 1)))
  }


  // 공재귀(co-recursive)함수 : 다음요소를 계속 생산하는 함수 (guarded recursion 보호되는 재귀 , 생산성을 공종료(co-termination)
  // 옵션안에는 (다음값, 다음상태)의 튜플이 있고, 상태로 Some이 나오면 계속 진행,
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty //종료
    }
  }

  val onesViaUnFold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  //0, 1, 1, 2, 3,5, 8...
  def fibsViaUnfold: Stream[Int] =
  //    unfold((0, 1))(s => Some(((s._1), (s._2, s._1 + s._2))))
    unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

}


object StreamExamples {

  def main(args: Array[String]) {
    //    val x = Cons( () => expensive(x), tl)
    //    val h1 = x.headOption
    //    val h2 = x.headOption
    //    val x = Stream.cons(expensive(x), tl)

    //    println(Stream(1, 2, 3).toList)
    //    println(Stream(1, 2, 3).take(2).toList)
    //    println(Stream(1, 2, 3).drop(0).toList)
    //    println(Stream(1, 2, 3).drop(1).toList)
    //    println(Stream(1, 2, 3).drop(2).toList)
    //    println(Stream(1, 2, 3).drop(3).toList)
    //    println(Stream(1, 2, 3).takeWhile(_ < 3).toList)
    //    println(Stream(1, 2, 3).takeWhileViaFoldRight(_ < 3).toList)
    //
    //    println(Stream(1, 2, 3).existsViaFoldRight(_ == 3))
    //    println(Stream(1, 2, 3).forAll(_ < 4))
    //
    //    println(Stream(1, 2, 3).headOption)
    //    println(Stream(1, 2, 3).headOptionViaFoldRight)
    //    println(empty.headOptionViaFoldRight)

    //    println(Stream(1, 2, 3).map(_ * 3).toList)
    //    println(Stream(1, 2, 3).filter(_ % 2 == 1).toList)
    //    println(Stream(1, 2, 3).append(Stream(4, 5)).toList)


    //    ones.toList //infinite loop
    // but,
    //    println(ones.take(5).toList)
    //    println(ones.exists(_ % 2 != 0))
    //
    //    println(ones.map(_ + 1).exists(_ % 2 == 0))
    //    println(ones.takeWhileViaFoldRight(_ != 1).toList)
    //    println(ones.forAll(_ != 1))
    //
    //    println(constant(3).take(5).toList)
    //
    //    println(from(10).take(100).toList)

    //    println(fibs().take(10).toList)
    //
    //    println(unfold())
    //
    //    println(onesViaUnFold.take(5).toList)
    //    println(fibsViaUnfold.take(10).toList)
    //    println(fromViaUnfold(100).take(10).toList)
    //    println(constantViaUnfold(100).take(10).toList)

    println(fromViaUnfold(100).takeWhileViaFoldRight(_ < 110).toList)

    val s = Stream(1, 2, 3)
    println(s.startsWith(Stream(1, 3)))
    println(s.startsWith(Stream(1, 2)))

    println(s.tails.toList.map(_.toList))

    println(s.hasSequence(Stream(3, 4)))
    println(s.hasSequence(Stream(2, 3)))

    println(Stream(1, 2, 3).scanRight(100)(_ + _).toList)
  }
}
