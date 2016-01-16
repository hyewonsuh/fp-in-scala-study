package ch7

import java.util.concurrent._


/**
  * Created by Wendy on 2016. 1. 12..
  */
object Par {

  //Type alias
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true

    override def isCancelled: Boolean = true

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
      Map2Future(af, bf, f)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  //7.4
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  //7.3
  //  In order to respect timeouts, we'd need a new `Future` implementation
  // that records the amount of time spent evaluating one future,
  // then subtracts that time from the available time allocated for evaluating the other future.
  case class Map2Future[A, B, C](af: Future[A], bf: Future[B], f: (A, B) => C) extends Future[C] {

    @volatile var cache: Option[C] = None

    override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = af.cancel(mayInterruptIfRunning) || bf.cancel(mayInterruptIfRunning)

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    override def isDone: Boolean = cache.isDefined

    def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = af.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = bf.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  // f: A => B   g:Par[A] => Par[B] f를 받아 g를 돌려주는 함수로 승급(lift)
  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  //    map2(parList, unit(()))((a, _) => a.sorted)

  // N개의 병렬계산 결합 : 새로운 기본수단으로 구현여부
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    //
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //7.5
  // 재귀호출을 fork를 감싸서 다른 논리적 쓰레드에서 실행됨을 표시하게 하였으나,
  // map2의 구현은 반으로 나누어 병렬로 실행하게 되어 있으므로, 오른쪽으로 계속 호출되면 성능면에서 효율이 없음
  def sequence_bad[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequence_bad(t)))((a, b) => a :: b)
  }
  // 시퀀스를 반으로 나누어 재귀 호출하도록.
  // fork로 감쌌기 때문에 입력목록이 크더라도 바로 리턴됨
  // run이 호출되면, 1개의 비동기 계산 -> N개의 병렬계산을 띄우고 모두 완료되면 하나의 목록으로 취합함.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)


  //7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {

    val pars: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  //max in IndexedSeq

  def main(args: Array[String]) {

//    val filter: Par[List[Int]] = Par.parFilter(List(1))(a => a> 2)


    val executorService: ExecutorService = Executors.newFixedThreadPool(1)
//    println(filter(executorService).get(2, TimeUnit.SECONDS))
    // 이 예제는 thread deadlock이 무조건 생김 // 개수가 4까지는 ..

//    println(Par.fork(unit(1))(executorService).get(2, TimeUnit.MINUTES)) //1개로 가능

//    println(Par.map2(lazyUnit(List(1,2)), lazyUnit(List(3,4)))(_ ++ _)(executorService).get(2, TimeUnit.MINUTES))//1개로 가능
    println(Par.sequence(List(lazyUnit(1)))(executorService).get(2, TimeUnit.MINUTES))

    executorService.shutdownNow()


  }

}
