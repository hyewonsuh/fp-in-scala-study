package ch7_01

import java.util.concurrent.TimeUnit


object Par {

//  type Par[A] = A => A

  //즉시 평가되어 a를 산출하는 계산을 생성
  def unit[A](a: => A): Par[A] = ???

  def get[A](a: Par[A]): A = ???

  //  def sum(ints: IndexedSeq[Int]): Int = {
  //    if (ints.size <= 1) ints.headOption getOrElse 0
  //    else {
  //      val (l, r) = ints.splitAt(ints.length / 2)
  //      val sumL: Par[Int] = unit(sum(l))
  //      val sumR: Par[Int] = unit(sum(r))
  //      // left 합이 구해져야 right를 실행하는 문제. (함수의 인수들이 왼쪽에서 오른쪽으로 평가되므로
  //      // 단순히 나열하면 병렬실행이 될 수 없다
  //      // unit이 비동기계산을 나타내는 Par[Int]를 get에 넘겨주면, get이 완료될때까지 unit의 실행이 차단되는 부수효가가 드러남
  //      // 비동기 계산의 조합이 필요함.
  //      get[Int](sumL) + get[Int](sumR)
  //    }
  //  }

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
  }

  // 두 병렬계산의 결과를 이항함수로 조합
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

  def sumTracing(args: Array[String]) {
    sum(IndexedSeq(1, 2, 3, 4))

    map2(
      sum(IndexedSeq(1, 2)),
      sum(IndexedSeq(3, 4))
    )(_ + _)

    map2(
      map2(
        sum(IndexedSeq(1)),
        sum((IndexedSeq(2)))
      )(_ + _),
      sum(IndexedSeq(3, 4))
    )(_ + _)

    map2(
      map2(
        map2(
          unit(1),
          unit(1)
        )(_ + _),
        sum((IndexedSeq(2)))
      )(_ + _),
      sum(IndexedSeq(3, 4))
    )(_ + _)

    // sum(IndexedSeq(1, 2)) 가 완전히 전개되어야 sum(IndexedSeq(3, 4)) 의 평가가 시작된다.
    // map2를 엄격하게 유지하되 실행이 즉시 시작되지 않게.........즉  병렬로 계산해야 할 것의 서술만 구축하하는 것을 의미.
    //  ==> 즉 평가( 예 get)전에는 아무일도 일어나지 않음
    //  ==> 서술을 엄격하게 구축하면 서술을 나타내는 객체가 상당히 무거운 개체가 됨. ==> map2를 게으르게만들고
    // , 양변을 병렬로  즉시 실행하게
    Par.map2(Par.unit(1), Par.unit(1))(_ + _) // 이런건 굳이 병렬 계산이 필요없다.. 금방 계산되므로..
    //계산을 주 thread로부터 분기하는 시점(forking) 을 알려줄 필요가 있음

  }

  def sum2(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _) // fork 로 인해 병렬성 제어를 밖으로 뺌...
    }
  }

  //이후에 run이 동시적으로 평가할 계산임을 표시함
  def fork[A](a: Par[A]): Par[A] = ???

  // run이 동시적으로 평가할 a를 감싼다.
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  //주어진 Par를 fork 요청에 따라 병렬 계산들을 수행하고 그 결과 값을 추출함으로써 완전히 평가함 .
//  def run[A](a: Par[A]): A = ???

//  def run[A](s: ExecutorService)(a: Par[A]): A

  def run[A](s:ExecutorService)(a: Par[A]):Future[A] = ??? //a(s)
  type Par[A] = ExecutorService => A
}


object DivideAndConquerExamples {

  def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1) ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }
  }


  def main(args: Array[String]) {

    println(sum(Range(1, 10)))
    println(Range(1, 10).length)
  }
}

/*
동시성 기본수단을 직접적으로 사용할
 */
trait Runnable {
  def run: Unit
}

class Thread(r: Runnable) {
  def start: Unit = ???

  //r의 실행을 시작
  def join: Unit = ??? //호출한 쓰레드의 실행은 r의 실행이 끝날때까지 차단.
}

// ==> 의미있는 값을 돌려주지 않음.
//     Runnable객체는 항상 그 내부행동방식에 대해 뭔가를 알아야 해서 일반적(generic)스타일로 다룰수 없다.
//     Thread 는 운영체제 thread에 직접 대응됨 => 논리적 쓰레드 를 도입해서 생성이 자유롭고 실제 os 쓰레드에 대응시키는 것이 좋음.

/*
 개선된 java.util.concurrent.Future, ExecutorService
 */
class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}

trait Callable[A] { def call: A}

trait Future[A] {
  def get: A
  def get(timeout: Long, unit:TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancel: Boolean
}

// 한계점.
// 물리적 쓰레드는 추상화 됨. future.get를 호출하면 호출한 쓰레드는 ExecutorService 의 실행이 끝날때까지 블럭됨.
//  future 합성하는 수단이 없음.

