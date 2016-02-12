sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](head: ()=> A, tail: ()=> Stream[A]) extends Stream[A]

object Cons{
  def cons[A](hd: => A, tl: => Stream[A]):Stream[A] = {
    lazy val h = hd
    lazy val t = tl
    Cons(() => h, () => t)
  }
}


def twice(i: () => Int) : Int = {
  val j = i()
  j + j
}

twice({() => println("hi"); 1})


def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A):A =
  if(cond) onTrue() else onFalse()

if2(true, { () => println("true");1}, { () => println("false");2})

def if3[A](cond: Boolean, onTrue: => A, onFalse: => A):A =
  if(cond) onTrue else onFalse

if3(true, {println("true");1}, { println("false");2})


class Normal[A](a: => A){
  val x = a
}
class Lazy[A](a: => A){
  lazy val x = a
}

val n =new Normal({println("hi one");1})  //a is evaluated!

val l = new Lazy({println("hi one");1})
l.x ////a is evaluated!



