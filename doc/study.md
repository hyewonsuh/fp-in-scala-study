## 01. Functional Programing ?
#### Pure Function
function without side-effect

#### side-effect
- 변수 수정, 자료구조 in place 수정
- obj.setA(b)
- throw exception
- IO

pure function =>  modularity 증가 => test, reuse, parallelism, 일반화, 분석이 쉬움, 버그가 적음.

side effect 를 최대한 바깥으로 밀어내어야 core logic 를 순수하게 지킬수 있다

#### Referential transparency (RT)
- expression (하나의 결과로 평가되는 임의의 코드조각) 의 속성임.
````
2 + 3 = 5
5 = 5   
````
- a라는 모든 표현식을 a의 평가결과로 치환해도 의미가 변하지 않으면, 그 표현식은 참조에 투명하다.
- f(x)가 참조에 투명한 모든 x에 대해 참조에 투명하면 함수 f는 pure 함.

#### Substitution model
- 모든 변수를 해당 값으로 치환하여 가장 간단한 형태로 환원
````scala
val x= "Hello World"
val r1 = x.reverse
val r2 = x.reverse
r1 == r2 //true
````

## 02.Getting started with FP in scala
꼬리 재귀함수/고차함수/다형적 고차함수

#### tail recursive function
````scala
def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc) //tail recursive --> compile while 문 byte code
    }
    go(n, 1)
}
````
#### parametric polymorphism
다형적 함수(형식의 추상화) : 임의의 형식에 대해 작동하는 함수.
````scala
  // A is Type Parameter (형식 매개변수)
def findFirst[A](ss: Array[A], key: A): Int = {
    @annotation.tailrec
    def loop(i: Int): Int = {
      if (i >= ss.length) -1
      else if (ss(i) == key) i
      else loop(i + 1)
    }
    loop(0)
}
````
스칼라의 함수 리터럴 : apply 메서드를 가진 하나의 객체가 됨.
````scala
val lessThan = new Function2[Int, Int, Boolean] {
  override def apply(v1: Int, v2: Int): Boolean = v1 < v2
}
val lessThan2 = (x:Int, y:Int) => x < y

lessThan(1,3)
lessThan.apply(1,3)
lessThan2(1,3)
lessThan2.apply(1,3)
````
#### 다형적 형식에서 도출된 구현
Type parameter 함수는 구현할수 있는 가능방법이 줄어듬. 심지어 구현이 딱 하나 존재할수도.
````scala
// a, b를 주면 c를 만들어줌. a는 이미 줬으니 b만 주면 c가 만들어짐
def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)
partial1(2, (x: Int, y: Int) => x * y)(3)

// a b로 c를 만들어줌, a 주고 나서. b도 받고. : 인자를 여러개 받는 함수를 인자를 하나씩 받을 수있는 chain으로 만듬.
def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
curry((a: Int, b: Int) => a * b)(2)(3)

//기존 함수를 currying
def sum(x:Int, y:Int) = x + y
val curriedSum = (sum _).curried  // (sum _) is function2
val plusOne = sum(1, _:Int) // partial applied function without currying
plusOne(1)
curriedSum(1)(1)

//uncurry
def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
uncurry(curriedSum)(2, 3)
Function.uncurried(curriedSum)(1,2)

````
함수 합성(composition) : 한 함수의 출력을 다른 함수의 입력으로.. 역시 signature로 결정됨
````scala
def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
// f compose g a == f(g(a))
// f andThen g a = g(f(a))
// f compose g == g andThen f
````

## 03. Functional Data structure
pure function 만으로 조작되는 자료구조이므로 (side effect이 없어야 하니) immutable 해야함.

값복사가 많이 일어나지 않을까? => 그렇지 않다 (functional data sharing)

````scala
val tail = List(2,3)
val xs = 1 :: tail

val ys = xs match {
  case (h :: tail) => 10 :: tail
  case Nil => Nil
}   
//tail data sharing
assert((tail eq xs.tail) == true) // AnyRef.eq 함수는 same reference인지 비교함
assert((tail eq ys.tail) == true)
````
Singly linked List example

````scala
sealed trait List[+A]
case object Nil extends List[Nothing] //
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
````

목록에 대한 재귀과 고차 함수로의 일반화

````scala

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =  as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))  
                       // f(x, f(x, f(x, f(x, foldRight(xs, z)(f)))))
    }

  val add = (a:Int,b:Int) => a + b
  foldRight(List(1,2,3),0)(add)
  // add(1,add(2, add(3, 0))) // 목록의 오른쪽 요소부터 f 에 적용
````

**ADT(algebraic data Type, 대수적자료형식)**  
>하나의 이상의 data construct 로 이루어진 타입. List의 경우 Nil 과 Cons(h, tail)  
>이런 타입을 data constructor 의 Sum Type(union, 합, 합집합) 이라 함 (List는 Nil과 Cons의 합)  
>이런 data constructor 를 인자들의 의 Product Type 이라고 함.  
>ADT described as 'sum of products types'

````scala
sealed abstract class Bool
case object True extends Bool
case object False extends Bool

sealed trait Tree[T]
case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T](value: T) extends Tree[T]
````

## 04. Handling errors without exceptions

throw exceptions  == side effect   
오류를 값으로 돌려주자  
부분함수 (함수내에서 일부입력에 대해 정의되지 않음. ex) `if empty list, throw exception`)를 완전함수로  
오류 처리 논리를 통하고 격리를 유지할 수 있음  
고차 함수를 이용해서 오류를 나타내는 값의 처리와 전파를 캡슐화

````scala
sealed trait Option[+A]{
  def map[B](f: A => B): Option[B] = this match {
    case None => None //None인 경우는 함수를 적용하지 않음.
    case Some(v) => Some(f(v)) //즉, 오류가 없는 경우만 계산을 진행함. f가 실패하면?
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

// B >:A B는 A와 같거나 A의 상위타입
  def getOrElse[B >: A](default: => B): B = this match { //default는 lazy paramter (실제로 쓰일때까지 평가되지 않음)
    case None => default
    case Some(v) => v
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
````

`o.getOrElse(throw new Exception("Fail"))` //복구 불가능할 경우 사용하는 관용구.  
모든 함수의 Option이 많아질까? => 보통의 함수를 Option 에 작용하는 함수로 승급(lift)시킬 수 있다.

````scala
  // a=>b 함수를 Option[a] => Option[B]로 승급시키는 함수
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
  lift((a: Int) => a.toString)(Some(3))
````

## 05. Strictness and laziness

while loop 를 쓰는대신 고수준 함수 합성을 추구 =>  non-strictness 는 함수의 효율성과 모듈성을 개선하는 근본적인 기법임
````scala
//각 변환은 새로운 임시목록을 생성하여 다음 변환의 입력으로만 쓰이고 폐기됨
List(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)
List(11, 12, 13, 24).filter(_ % 2 == 0).map(_ * 3)
List(12, 14).map(_ * 3)
List(36, 42)
````

**non-strictne**? 함수가 하나 이상의 인자를 평가하지 않는 것.

````scala
square(41.0 * 1.0 ) // 인수를 평가한 후 실행 (엄격한 함수)
// 이미 알고 있는 non-strictness 예.
println(false && {
  println("!!"); true //실행안됨
})
````

scala 의 non-strictness 함수 작성 방법
- **=> A** thunk (썽크) : 표현식이 평가되지 않은 상태

````scala
// i의 평가     
def maybeTwice(b:Boolean, i: => Int) = if(b) i + i else 0

scala> maybeTwice(true, {println("hi"); 1 + 2}) // twice evaluated.
hi
hi
res2: Int = 6


//비엄격함수의 인수는 값이 아니라  by name 으로 전달됨
def maybeTwice2(b:Boolean, i: => Int ) ={
lazy val j = i
if(b) j + j else 0 //처음 참조 될때 를 한번 평가 후 lazy변수인 j에 캐싱함.
}

scala> maybeTwice2(true, {println("hi"); 1 + 1})
hi
res0: Int = 4

````

>:warning: call-by-name: **=> A**  
 by name 으로 전달되는 parameter.  
 함수 내에서 사용된 패러미터의 이름에 대해 전달된 인수로 대체됨. (평가되지 않고)  
```scala
def if2[A](cond: Boolean, onTrue: => A, onFalse: => A):A = if(cond)  onTrue else onFalse  
if2(true, {println("true");1},  {println("false");2})  
````
 >`val` parameters may not be call-by-name  


>:warning:0-arity Functions(scala.Function0 type): **() => A**
````scala
def if3[A](cond: Boolean, onTrue: () => A, onFalse: () => A):A = if(cond) onTrue() else onFalse()
>>if3(true, { () => println("true");1}, { () => println("false");2})
````


- Lazy List Example

````scala
sealed trait Stream[+A]

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] //`val' parameters may not be call-by-name

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

````

- laziness 표현식의 서술과 평가의 분리  

## 06. Purely functional state
## 07. Purely functional parallelism
