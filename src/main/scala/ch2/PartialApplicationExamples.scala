package ch2

/**
  * Created by hyewon on 2015. 12. 26..
  */
object PartialApplicationExamples {

  // a, b를 주면 c를 만들어줌
  // a는 이미 줬으니 b만 주면 c가 만들어짐
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  // a b로 c를 만들어줌, a 주고 나서. b도 받고.
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)


  // function composition 함수의 출력이 다른 함수의 입력
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))


  def sum(x:Int, y:Int) = x + y

  def main(args: Array[String]) {

    println(partial1(2, (x: Int, y: Int) => x * y)(3))

    println(curry((a: Int, b: Int) => a * b)(2)(3))

    val curriedSum = (sum _).curried  // (sum _) is function2
    val plusOne = sum(1, _:Int)
    println(plusOne(1))
    println(curriedSum(1)(1))

    println(uncurry(curriedSum)(2, 3))
    println(Function.uncurried(curriedSum)(1,2))
    //
    //
    //    val f = (x: Double) => math.Pi / 2 - x
    //
    //    val cos = f andThen math.sin
    //
    //    println(f(10))
    //    println(cos(10))

    val f = (x: Int) => 1 + x
    val g = (x: Int) => 10 * x

    println((f andThen g) (10)) //  (1 + x ) * 10
    println((g compose f) (10)) // f(g(x))
  }

}
