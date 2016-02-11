package ch4

/**
  * Created by hyewon on 2016. 1. 1..
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  //    this.map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  //    this match {
  //      case Some(v) if f(v) => this
  //      case _ => None
  //  }

}


case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object OptionExample {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))


  // a=>b 함수를 Option[a] => Option[B]로 승급시키는 함수
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  //    oa => oa map f

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age + numberOfSpeedingTickets * 0.04 / 0.39

  def parseInsuranceRateQuote(
                               age: String,
                               numberOfSpeedingTickets: String): Option[Double] = {
    val optAge = Try(age.toInt)
    val optNumberOfSpeedingTickets = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optNumberOfSpeedingTickets)(insuranceRateQuote)
  }


  def map_my[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }

  //Use the `flatMap` and possibly `map` methods.
  //a가 None이면 바로 None리턴
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
//    a flatMap (aa =>
//      b map (bb =>
//        f(aa, bb)))
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  //Break the list out using pattern-matching where there will be a recursive call to `sequence` in the cons case.
  // Alternatively, use the `foldRight` method to take care of the recursion for you.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }


  //??
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  def parseInt(a:List[String]):Option[List[Int]] = {
    sequence( a map (s => Try(s.toInt)) )
  }

//  The `traverse` function can be written with explicit recursion or use `foldRight` to do the recursion for you.
  // Implementing `sequence` using `traverse` may be more trivial than you think.

  def traverse[A,B](a:List[A])(f:A=> Option[B]):Option[List[B]]= a match {
    case Nil => Some(Nil)
    case h :: t => map2( f(h), traverse(t)(f))( (a, b) => a :: b)
  }

  def main(args: Array[String]) {
    //    val lift: (Option[Int]) => Option[Int] = Some(1).lift((a:Int) => a + 2)
    //    val lifted: (Option[Int]) => Option[Int] = lift((a:Int) => a*3)
    //    println(lifted(Some(3)))

    //    val lifted: Option[String] = lift((a: Int) => a.toString)(Some(3))
    //    println(lifted)
    //
    //    println(math.abs(-2))
    //
    //    println(lift(math.abs)(Some(-3)))
    //    println(lift(math.abs)(None))

    //    println(parseInsuranceRateQuote("15", "2"))
    //    println(parseInsuranceRateQuote("15a", "21a"))

//    println(sequence(List(Some(1), Some(2), Some(3))))
    //    println(sequence(List(Some(1), None, Some(3))))

//    parseInt(List("1", "2", "3"))
//    println(traverse(List("1", "2", "3"))(s => Try(s.toInt)))


//    println(mean(List(1, 2, 3)))
//    println(variance(List(0)))

    println(lift(math.abs))

    List(1,2,3).fl
  }


}
