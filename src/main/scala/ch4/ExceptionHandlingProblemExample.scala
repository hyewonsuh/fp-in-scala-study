package ch4

/**
  * Created by hyewon on 2016. 1. 1..
  */
object ExceptionHandlingProblemExample {

  def failingFn(i: Int): Int = {
    //    val y: Int = throw new Exception("fail!") //throw error

    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    } catch {
      case e: Exception => 43
    }

  }

  def mean(xs: Seq[Double]): Double = {
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list")  //function mean is partial function (일부 입력에는 정의되지 않은 함수
    else xs.sum / xs.length
  }

  def mean_1(xs:Seq[Double], onEmpty:Double):Double = { //onEmpty 처리방식으로 호출자가 알아야 함.
    if(xs.isEmpty) onEmpty
    else xs.sum / xs.length
  }

  def main(args: Array[String]) {
    println(failingFn(12))
  }
}
