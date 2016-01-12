package ch2

/**
 * Created by hyewon on 2015. 12. 26..
 */
object TypeParameterExamples {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def sorted(i: Int): Boolean = {
      if (i >= as.length - 1) true
      else if (ordered(as(i), as(i + 1))) sorted(i + 1)
      else false

    }

    sorted(0)
  }

  def main(args: Array[String]) {

    val lessThan = new Function2[Int, Int, Boolean] {
      override def apply(v1: Int, v2: Int): Boolean = v1 < v2
    }

    lessThan(1,3)
    lessThan.apply(1,3)

    isSorted(Array(1, 3, 2), lessThan)
    //
    //    def compare(x: Int, y: Int): Boolean = {
    //      x > y
    //    }
    //    isSorted(Array(1, 3, 2), compare)

    //array literal       function literal
    println(isSorted(Array(1, 3, 2), (x: Int, y: Int) => x < y))
    println(isSorted(Array(1, 2, 4, 5, 6, 7), (x: Int, y: Int) => x < y))
    println(isSorted(Array(1, 2, 4, 5, 6, 7), (x: Int, y: Int) => x > y))
  }

}
