package ch3_1
/**
  * Created by Wendy on 2016. 1. 15..
  */
object ScalaListExamples {

  def main(args: Array[String]) {

    val tail = List(2,3)
    val xs = 1 :: tail

    val ys = xs match {
      case (h :: tail) => 10 :: tail
      case Nil => Nil
    }

    assert((tail eq xs.tail) == true)
    assert((tail eq ys.tail) == true)
  }

}
