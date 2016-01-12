package ch4

/**
  * Created by hyewon on 2016. 1. 2..
  */

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
  //    this flatMap ( aa => b map (bb => f(aa, bb)))
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

  def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (Left(e), _) => Left(e)
      case (_, Left(ee)) => Left(ee)
      case (Right(a), Right(b)) => Right(f(a, b))
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]

// E is error
case class Right[+A](value: A) extends Either[Nothing, A]

// right is success
object EitherExample {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int) = ???

  def parseInsuranceRateQuote(age: String, numOfSpeedingTickets: String) = {

    for {
      a <- Try {
        age.toInt
      }
      b <- Try {
        numOfSpeedingTickets.toInt
      }
    } yield insuranceRateQuote(a, b)
  }

  // def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  // A is Either[E,A]
  // b is Either[List[A]]
  // (a,b) => c
  //  === (A, List[A) => List[A])
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case head :: tail => (f(head) map2 traverse(tail)(f)) (_ :: _)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(a => a)

  def mkName(name: String): Either[String, Name] = {
    if (name == "" || name == null) Left("Name is empty")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either[String, Age] = {
    if (age < 0) Left("Age is out of range")
    else Right(new Age(age))
  }

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)
