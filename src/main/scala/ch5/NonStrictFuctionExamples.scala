package ch5

/**
  * Created by hyewon on 2016. 1. 3..
  */
object NonStrictFuctionExamples {

  def main(args: Array[String]) {

    //각 변환은 새로운 임시목록을 생성하여 다음 변환의 입력으로만 쓰이고 폐기됨
    List(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)
    List(11, 12, 13, 24).filter(_ % 2 == 0).map(_ * 3)
    List(12, 14).map(_ * 3)
    List(36, 42)

    //Non-strictness은 함수적 프로그램의 효율성과 모듈성을 개선하는 근복적인 기법임
    // 비엄격성 => 함수가 하나 이상의 인수를 평가하지 않을수도 있다는 뜻


    square(41.0 * 1.0 ) // 인수를 평가한 후 실행 (엄격한 함수)

//    square(sys.error("fail")) //square실행전에 에러가 남

    //이미 알고 있는 비엄격 개념
    println(false && {
      println("!!"); true //실행안됨
    })

    if2(false, sys.error("Fail"), 3 + 3)

    val x = maybeTwice(true, {println("hi"); 1 + 41})
    println(x)

    val x2 = maybeTwice2(true, {println("hi"); 1 + 41})
    println(x2)

  }
  def square(x: Double): Double = x * x

  //non-strict function
  // () => A thunk (썽크) 표현식이 평가되지 않은 상태
  def if2[A](cond: Boolean, onTrue: => A, onFalse:  => A):A =
//  def if2[A](cond: Boolean, onTrue: ()=> A, onFalse: () => A):A =
//    if(cond) onTrue() else onFalse()
    if(cond)
      onTrue
    else
      onFalse

  def maybeTwice(b:Boolean, i: => Int) = if(b) i+i else 0 //i 를 두 번 평가함

  //비엄격함수의 인수는 값이 아니라  by name 으로 전달됨

  def maybeTwice2(b:Boolean, i: => Int ) ={
    lazy val j = i
    if(b) j + j else 0 //i를 한번 평가 후 lazy변수인 j에 캐싱함.
  }
}
