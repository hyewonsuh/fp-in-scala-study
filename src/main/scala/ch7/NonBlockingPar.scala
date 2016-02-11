package ch7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}


/**
  * Fully non-blocking 방식의 Par
  * - Future의 blocking 메서드 (get)를 호출하지 않아야 함.
  * - 이렇게 구현되었음을 검사하는 데 사용할수 있는 법칙이 있다.
  *
  * - 우선은 적당한 때에 호출되는 콜백을 등록하는 방식.
  *
  *
  */

object Nonblocking {

  sealed trait Future[+A] {
    // k 는 콜백함수(continuation 함수)
    // 패키지 내에서만 접근가능 API 순수성 유지.
    // k는 리턴이 없는데 ... 부수효과가 client 에게 노출되지 않는 일종의 기법으로 봐야함.. local effect
    private[ch7] def apply(k: A => Unit): Unit
  }

  //object NonBlockingPar {
  type Par[+A] = ExecutorService => Future[A]

  //이전과 동일하나, 새로 정의한 Future를 사용함.

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A] //결과를 저장하는 thread-safe val
      val latch = new CountDownLatch(1)
      //      p(es).apply( a => ref.set(a); latch.countDown())
      p(es) {
        a => ref.set(a); latch.countDown //k
      }
      latch.await()
      ref.get
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a) //값을 콜백함수에 전달.
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }

    /**
      * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
      * This will come in handy in Chapter 13.
      */
    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      def apply(cb: (A) => Unit): Unit = f(cb)
    }

    // a(es)(k)는 Future[A] 를 산출
    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(k: (A) => Unit): Unit = eval(es)(a(es)(k))
      }

    //비동기 계산을 평가하는 helper function (es.submit ~~ r)
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        def call = r
      })

    // specialized version of `map`
    def map[A, B](p: Par[A])(f: A => B): Par[B] = es =>
      new Future[B] {
        def apply(cb: B => Unit) = p(es)(a => eval(es)(cb(f(a))))
      }

    def map2[A, B, C](ap: Par[A], bp: Par[B])(f: (A, B) => C): Par[C] =
    // a 평가 b평가 후 , 결과를 k(f(a,b))
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None // result of a
          var br: Option[B] = None // result of b

          // 두 결과를 기다렸다가 f로 결합해서 cb에 넘겨주는 행위자
          val combiner = Actor[Either[A, B]](es) {
            //handler
            case Left(a) => br match {
              //a의 결과가 나오면
              case None => ar = Some(a) //b가 아직이면 ar에 담고
              case Some(b) => eval(es)(cb(f(a, b))) // b도 결과가 나왔으면 f(a,b)를 callback에 전달
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b))) //b가 먼저 나오면.
            }
          }
          ap(es)(a => combiner ! Left(a))
          bp(es)(b => combiner ! Right(b))
        }
      }

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

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

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    //cond가 true이면 t, false이면 f를 사용해서 계산
    //    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    //      es => if ((run(es)(cond))) t(es) else f(es) //cond의 결과가 나올때까지 차단됨.

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = {
          println("starting callback...")
          cond(es) { b =>
            print("cond result is " + b)
            if (b) t(es)(cb)
            else f(es)(cb)
//
//            if (b) eval(es) (t(es)(cb))
//            else eval(es) (f(es)(cb))
          }
        }
      }

    def choiceViaChoiceN[A](cond: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    // cond 는  Par[Boolean] => Par[Int]로 변환. map이용
      choiceN(map(cond)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => {
        new Future[A] {
          def apply(cb: A => Unit) = {
            n(es) {
              i => eval(es)(choices(i)(es)(cb))
            }
          }
        }
      }

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => {
        new Future[V] {
          def apply(cb: V => Unit): Unit = {
            key(es) {
              k => {
                //               val selectedPar: Par[V] = choices(k)
                //               val future: Future[V] = selectedPar(es)
                //               eval(es)(future(cb))
                eval(es)(choices(k)(es)(cb))
              }
            }
          }
        }
      }

    // choice, choiceN, choiceMap 의 일반화된 조합기
    // Map[A, Par[B]]   is  A => Par[B]
    // List[Par[B]]     is  Int => Par[B]
    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      flapMap(pa)(choices)
//      es => {
//        new Future[B] {
//          def apply(cb: B => Unit): Unit =
//            pa(es) {
//              a => choices(a)(es)(cb)
//            }
//        }
//      }

    def flapMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B]{
        def apply(cb:B =>Unit):Unit =
          pa(es)(a => f(a)(es)(cb))
      }

    def join[A](a: Par[Par[A]]):Par[A] = {
      es => new Future[A]{
        def apply(cb:A =>Unit):Unit =
          a(es)(aa => aa(es)(cb))
      }
    }
    //    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    //      es => {
    //        val idx: Int = run(es)(n)
    //        run(es)(choices(idx))
    //      }
  }


  def main(args: Array[String]) {

    val es: ExecutorService = Executors.newFixedThreadPool(3)
    //
    //    val forkedPar: Par[Int] = Par.fork(Par.unit(100))
    //    forkedPar(es)(i => {
    //      Thread.sleep(2000)
    //      println("forked Par" + (i * 100))
    //    }) //비동기 콜만 하고 즉시 반환.
    //
    //    val p: Par[Int] = Par.unit(100)
    //    p(es)(i => println("unit Par " + i * 100))

    //    val parMap1: Par[List[Double]] = Par.parMap(List.range(1, 100000))(math.sqrt(_))
    //    val x: List[Double] = Par.run(es)(parMap1)
    //
    //    print(x)


    es.shutdown()
  }
}

