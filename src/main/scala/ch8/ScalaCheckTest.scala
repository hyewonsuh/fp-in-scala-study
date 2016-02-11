package ch8

import org.scalacheck.Gen
import org.scalacheck.Prop._

/**
  * Created by hyewon on 2016. 2. 7..
  */
object ScalaCheckTest extends App {

  val intList = Gen.listOf(Gen.choose(0, 100))

  val prop =
    forAll(intList)(ns => ns.reverse.reverse == ns) &&
      forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

  val failingProp =
    forAll(intList)(ns => ns.reverse == ns)

  prop.check
  failingProp.check

  /**
    * sum:List[Int] => Int
    * - empty list => 0
    * - sum == sum of reverse list
    * - sum/n = n * list(0)
    * - sum(a,b,c,d,e) = sum(a,b) + sum(c,d,e)
    * - sum(1..n) = n(n+1) /2
    *
    * max:List[Int] => Int
    * - n == 1 => max(list) = list(0)
    * - max(list) >= 모든 원소에 대해
    * - empty list =>  return None or throw Error
    *
    */
}
