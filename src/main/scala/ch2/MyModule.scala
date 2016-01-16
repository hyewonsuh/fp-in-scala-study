package ch2

/**
 * Created by hyewon on 2015. 12. 26..
 */
object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc) //tail recursive --> compile while 문 byte code
    }
    go(n, 1)
  }


  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

  }

  private def formatFactorial(n: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(n, factorial(n))
  }

  // high order function
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  // 다형적 함수(형식의 추상화) parametric polymorphism
  def findFirst[A](ss: Array[A], key: A): Int = {

    @annotation.tailrec
    def loop(i: Int): Int = {
      if (i >= ss.length) -1
      else if (ss(i) == key) i
      else loop(i + 1)
    }
    loop(0)
  }

  def findFirst[A](ss: Array[A], p: A => Boolean): Int = {

    @annotation.tailrec
    def loop(i: Int): Int = {
      if (i >= ss.length) -1
      else if (p(ss(i))) i
      else loop(i + 1)
    }
    loop(0)
  }


  def main(args: Array[String]) {
    //    println(formatAbs(-42))
    //    println(formatFactorial(7))
    println(formatResult("abc", -42, abs))
    println(formatResult("factorial", 7, factorial))

    println(findFirst(Array(7, 9, 13), 9))
    println(findFirst(Array("32", "44", "234"), "234"))
    println(findFirst(Array("32", "44", "234"), "233"))

    println(findFirst(Array("32", "44", "234"), (s:String) => s.length == 3))
  }


}
