package ch2

/**
 * Created by hyewon on 2015. 12. 26..
 */
object TailRecursiveExamples {

  def factorial(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {

      if (n <= 0) acc
      else go(n - 1, n * acc) //tail recurcive --> compile while byte code
    }
    go(n, 1)
  }

  //0, 1, 1, 2, 3, 5...n-2, n-1, n

  // fib(1) =  0
  // fib(2) =  1
  // fib(3) = fib(1) + fib(2)
  def fibonacci(n: Int): Int = {

    def go(n: Int): Int = {
      if (n == 1) 0
      else if (n == 2) 1
      else
        go(n - 1) + go(n - 2)
    }
    go(n)
  }

  def fiboLoop(n:Int): Int = {
    var prePreValue = 0
    var preValue = 1
    var nValue = 0

    if (n == 1) prePreValue
    else if (n == 2) preValue
    else{
      for (i <- 3 to n + 1) {
        prePreValue = preValue
        preValue = preValue + prePreValue
      }
      preValue;
    }
  }

  // fib(1) =  0
  // fib(2) =  1
  // fib(3) = fib(2) + fib(1) 1
  // fib(4) = fib(3) + fib(2) 2
  // fib(5) = fib(4) + fib(3) 3
  // fib(6) = fib(5) + fib(4) 5
  /*
  You will definitely need a helper method like we did with `factorial`.
  But think about what information you need at each iteration.
  You might need two values, one for each of the two numbers you need to calculate the next number.
  And you know the first two numbers already.
  Note that the nth Fibonacci number has a closed form solution
  (see http://en.wikipedia.org/wiki/Fibonacci_number#Closed-form_expression).
  Using that would be cheating; the point here is just to get some practice writing loops with tail-recursive functions.
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, cur:Int): Int = {
      if (n == 1) prev //0
      else
        go(n-1, cur, prev + cur)

    }
    go(n, 0, 1)
    /*
      go(2, 0, 1) -> go(1, 1, 1)
      go(3, 0, 1) -> go(2, 1, 1)
      go(4, 0, 1) -> go( 4 3 2 1, 1 2 , 0 + 1 + 1 +
    */
  }

  def main(args: Array[String]) {
    //    println(formatAbs(-42))
    //    println(factorial(14))
    println(fibonacci(6))
    //    println(fib(10))
    assertFib
  }

  def assertFib(): Unit ={
    testFib(fib)
    testFib(fiboLoop)
    //    testFib(fibonacci)
  }


  def testFib(f: Int => Int ) = {
    assert( 0== (f(1)))
    assert( 1== (fib(2)))
    assert( 1== (fib(3)))
    assert( 2== (fib(4)))
    assert( 3== (fib(5)))
    assert( 5== (fib(6)))
    assert( 8== (fib(7)))
  }
}
