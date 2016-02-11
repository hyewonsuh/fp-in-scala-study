package ch8

import ch5.Stream
import ch6.{RNG, State}
import ch8.Prop.{Result, TestCases}

/**
  * Created by hyewon on 2016. 2. 7..
  */

//A 타입의 값을 생성하는 방법은 아는 어떤 것
//랜덤 생성 State is S => (A, S)
case class Generator[A](sample: State[RNG, A]) {

  // 생성된 값들에 의존하는 생성기 (0..11 사이의 정수를 선택한 후, 그 값을 길이로 하는 목록을 생성한다던가)
  def flatMap[B](f: A => Generator[B]): Generator[B] = {
    val state: State[RNG, B] = sample
      .flatMap(a => f(a)
        .sample)
    Generator(state)
  }

  def listOfN(size: Generator[Int]): Generator[List[A]] = {
    size
      .flatMap(n => Generator
        .listOfN(n, this))
  }
}

object Generator {
  def choose(start: Int, stopExclusive: Int): Generator[Int] = {
    Generator(State(RNG
      .nonNegativeInt)
      .map(i => start + i % (stopExclusive - start)))
  }

  def unit[A](a: => A): Generator[A] = Generator(State
    .unit(a))

  def boolean: Generator[Boolean] = Generator(State(RNG
    .boolean))

  def listOfN[A](n: Int, g: Generator[A]): Generator[List[A]] =
    Generator(State
      .sequence(List
        .fill(n)(g
          .sample)))

  def union[A](g1: Generator[A], g2: Generator[A]): Generator[A] =
    boolean
      .flatMap(b => if (b) g1 else g2)

  //(g1, w1) (g2, w2) w1 , w1 + w2 = t  w1/(w1+w2) =
  def weighted[A](g1: (Generator[A], Double), g2: (Generator[A], Double)): Generator[A] = {
    val g1Threshold = g1
      ._2
      .abs / (g1
      ._2
      .abs + g2
      ._2
      .abs)
    Generator(State(RNG
      .double)
      .flatMap {
        d => if (d > g1Threshold) g2
          ._1
          .sample
        else g1
          ._1
          .sample
      })
  }

}


//trait Prop {
//combine Prop
//  def &&(p: Prop): Prop =
//    new Prop {
//      override def check: Boolean = Prop.this.check && p.check
//    }
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//}


case class Prop(run: (TestCases, RNG) => Result)

//

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  //검사 통과로 인정할수 있는 test 개수

  sealed trait Result {
    def isFalsified: Boolean //falsify 거짓임음 입증하다.
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def buildMessage[A](a: A, e: Exception): String =
    s"test case: $a\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Generator[A])(f: A => Boolean): Prop =
    Prop {
      (tcs, rng) =>
        randomStream(as)(rng)
          .zip(Stream.from(0)) // Stream[(randomValue, index)]
          .take(tcs) //testcase count
          .map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMessage(a, e), i)
          }
        }.find(_.isFalsified)
          .getOrElse(Passed)
    }

  def randomStream[A](g: Generator[A])(rng: RNG): Stream[A] =
    Stream
      .unfold(rng)(rng => Some(g
        .sample
        .run(rng)))
}

object PropBasedTesting {

  def listOf[A](a: Generator[A]): Generator[List[A]] = ???

  def listOfN[A](n: Int, a: Generator[A]): Generator[List[A]] = ???


  def main(args: Array[String]) {
  }

}
