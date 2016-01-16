package ch7

/**
  * Created by hyewon on 2016. 1. 13..
  */
object ApiAlgebra {


  def main(args: Array[String]) {

    //1. map의 법칙
    //
    List(1, 2, 3).map(_ + 1) == List(2, 3, 4)


    //
    List(1, 2, 3).map(f) == List(f(1, 2, 3))
    //f를 항등함수로 치환.
    List(1, 2, 3).map(id) == List(id(1, 2, 3))
    List(1, 2, 3).map(id) == List(1, 2, 3)

    val y = List(100, 200)

    //map 은 단지 y의 결과를 f에 적용 . f가 id면 아무런 영향을 미치지 않는다.
    y.map(id) == y

    y.map(f).map(f)


    /*
      * y.map(id) == y
      * y.map(id) == id(y)
      * y.map(f) == f(y)
      * map의 parametricity 덕에 생긴 공짜 정리 (free theorem)
      * http://ttic.uchicago.edu/~dreyer/course/papers/wadler.pdf
      */

    //2. fork에 관한 법칙 ==> delay
    /*
    For a thread pool of size 2, `fork(fork(fork(x)))` will deadlock, and so on.
    Another, perhaps more interesting example is `fork(map2(fork(x), fork(y)))`.
    In this case, the outer task is submitted first and occupies a thread waiting for both `fork(x)` and `fork(y)`.
    The `fork(x)` and `fork(y)` tasks are submitted and run in parallel, except that only one thread is available,
    resulting in deadlock.
    */
  }

  def f[A](a: A): A = a

  def g[A](a: A): A = a

  //identity function
  def id[A](a: A): A = a
}
