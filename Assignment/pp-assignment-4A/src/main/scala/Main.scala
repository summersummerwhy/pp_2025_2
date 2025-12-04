package pp202402.assign4a

import scala.annotation.tailrec
import scala.util.control.TailCalls._

/** Principles of Programming: Assignment 4A.
  *
  * Implement given functions, which are currently left blank. (???) **WARNING:
  * Please read the restrictions below carefully.**
  *
  * If you do not follow these, **your submission will not be graded.**
  *
  *   - Do not use the keyword `var`. Use `val` and `def` instead.
  *   - Do not use any library functions or data structures like `LazyList`,
  *     `Array`, `Range` (`1 to n`, `1 until n` ...), `fold`, `map`, `reduce` or
  *     etc.
  *   - If you want to use a data structure, create new one instead of using the
  *     library ones.
  *   - You can only use tuples, `scala.annotation.tailrec`,
  *     `scala.util.control.TailCalls._`, and 
  *     `List`, `Nil`, `Cons` (to implement `toList` function) from the library.
  *   - Do not use any looping syntax of Scala (`for`, `while`, `do-while`,
  *     `yield`, ...)
  *
  * Again, your score will be zero if you do not follow these rules.
  *
  * We do not require tail-recursion explicitly for this assignment.
  * 
  * All testcases for grading will include both instances of problem 1-1 and functions of problem 1-2. 
  * That means, you may get 0 points for this assignment if you only solve problem 1-1 (or 1-2).
  *
  * Timeout: 30 sec.
  */

object Lazylist{
  import Data.* 
/**
  * Problem 1-1
  * 
  * Implement instances of Iter and ListIF for Lazylist.
  */
  sealed abstract class Lazylist[A] {
    def get : Option[(A, Lazylist[A])]
  }
  class LNil[A]() extends Lazylist[A] {
    def get = None
  }
  class LCons[A](hd: A, _tl: => Lazylist[A]) extends Lazylist[A] {
    private lazy val tl = _tl
    def get = Some(hd,tl)
  }

  // List[A] <: Iter[A] 완료
  // lazylist[A] <: Iter[A]을 해야함 -> Data 참고하면 될듯
  implicit def lazylistIter[A]: Iter[Lazylist,A] = new {
    def get[A] (l: Lazylist[A]) =
      l.get match {
        case None => None
        case Some((hd, tl)) => Some(hd, tl)
      }
  }

  // 이제 lazylistListIF[A] <: ListIF[A]
  // 아닌 것 같고 일단 구현해보겠음....
  implicit def lazylistListIF[A]: ListIF[Lazylist,A] = new {
    def empty[A] = LNil[A]()
    def cons[A] (a: A, l: =>Lazylist[A]) = LCons[A](a, l)
    def iter[A] = lazylistIter[A]
    def head[A] (l: Lazylist[A]) =
      l.get match {
        case None => None
        case Some(hd, tl) => Some(hd)
      }
    def tail[A] (l: Lazylist[A]) =
      l.get match {
        case None => LNil[A]()
        case Some(hd, tl) => tl
      }
  }
}


object Assignment4A:
  import Data.*
/**
  * Problem 1-2
  * 
  * Implement 3 functions below.
  * 
  * You don't need to worry about stack overflow or Long type overflow. 
  * All test cases for grading will be small enough. 
  *
  */
  
/** 
  * fibo() is a list of fibonacci sequence. 
  * 
  * ex): fibo = [0,1,1,2,3,5,8,...]
  *
  * See https://en.wikipedia.org/wiki/Fibonacci_sequence for more information.
  * 
  * remark : fibo[List] will cause error. 
  */
  def fibo[L[_]] (implicit listIF : ListIF[L,Long]) : L[Long] = {
    // cons(0, fibs(1, 1)) 호출
    // LCons(0, fibs(1,1)) 호출
    // 하지만 여기의 LCons는 =>, 그리고 안의 get에서 lazy val로 호출
    // 이 시점에도 fibs(1,1)은 그대로 전달됐다가
    // 누가 여기서 get을 부를때 fibs(1, 1) 평가 들어감
    // 다음꼬리에서도 반복
    def fibs(a: Long, b: Long): L[Long] =
      listIF.cons(a, fibs(b, a + b))

    fibs(0, 1)
  }

/** 
  * getN(l,n) is a list of first n elements of l.
  * 
  * ex): getN([1,2,3,4...], 3) = [1,2,3]
  */

  @tailrec def reverse[A, L[_]](l: L[A], result: L[A]) (implicit listIF : ListIF[L,A]): L[A] = {
    listIF.head(l) match {
      case Some(a) => reverse(listIF.tail(l), listIF.cons(a, result))
      case _ => result
    }
  }

  def getN[A, L[_]](l: L[A], n: Int) (implicit listIF : ListIF[L,A]) : L[A] = {

    @tailrec def getNInside(l2: L[A], m: Int, result: L[A]): L[A] = {
      if (m == n) result
      else
        listIF.head(l2) match {
          case Some(a) => getNInside(listIF.tail(l2), m + 1, listIF.cons(a, result))
          case _ => result
        }
    }

    // l, n을 parameter로 사용, listIF의 함수를 활용해서 구현
    if (n == 0) listIF.empty
    else
      // head를 n번빼내서 cons로 합쳐서 돌려준다
      val temp = getNInside(l, 0, listIF.empty)
      reverse(temp, listIF.empty)

  }

/** 
  * toIList(l) is a List version of given list l.
  */
  def toList[A, L[_]](x: L[A]) (implicit listIF : ListIF[L,A]) : List[A] = {
    // L[A]를 List[A]로 바꿔서?
    // 그러니까 더 좁은 버전으로.... 흠흠
    val temp = List[A]()
    val temp2 = List[A]()
    @tailrec def appendToList(l: L[A], result: List[A]): List[A] = {
      listIF.head(l) match {
        case Some(a) => appendToList(listIF.tail(l), a::result)
        case _ => result
      }
    }

    reverse(appendToList(x, temp), temp2)
  }