package ch7

import java.util.concurrent.ExecutorService

/**
  * Created by hyewon on 2016. 1. 13..
  */

sealed trait Future[A] {
  private[ch7] def apply(k: A => Unit): Unit
}

object NonBlockingPar {
  type Par[+A] = ExecutorService => Future[A]


}
