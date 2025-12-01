package pp202402.assign3a

import scala.annotation.tailrec
import scala.util.control.TailCalls._

/** Principles of Programming: Assignment 3A.
  *
  * Implement given functions, which are currently left blank. (???) **WARNING:
  * Please read the restrictions below carefully.**
  *
  * If you do not follow these, **your submission will not be graded.**
  *
  *   - Do not use the keyword `var`. Use `val` and `def` instead.
  *   - Do not use any library functions or data structures like `LazyListist`,
  *     `Array`, `Range` (`1 to n`, `1 until n` ...), `fold`, `map`, `reduce` or
  *     etc.
  *   - If you want to use a data structure, create new one instead of using the
  *     library ones.
  *   - You can only use tuples, `scala.annotation.tailrec`, and
  *     `scala.util.control.TailCalls._` from the library.
  *   - Do not use any looping syntax of Scala (`for`, `while`, `do-while`,
  *     `yield`, ...)
  *
  * Again, your score will be zero if you do not follow these rules.
  *
  * Note that these rules will be gradually relaxed through the next
  * assignments.
  *
  * We do not require tail-recursion explicitly for this assignment.
  *
  * Timeout: 30 sec.
  */
object Assignment3A:
  import Data.*
  /*
   * Problem 1
   *
   * List decimal fractional parts of sqrt(n), which means nonnegative k such that k^2 = n. Digits should be truncated by `len`.
   * 
   * Input will be given as LazyList. For simplicity, we restrict the input with 0.0 <= input < 10.0
   * 
   * You don't need to worry about overflow in Long Type.
   *
   * e.g. sqrt(2) = 1.41421...
   *      decSqrt([2], 5) = [1, 4, 1, 4, 2, 1]
   * 
   *      sqrt(0.7) = 0.83666...
   *      decSqrt([0, 7], 5) = [0, 8, 3, 6, 6, 6]
   * 
   *      sqrt(1.44) = 1.2
   *      decSqrt([1, 4, 4], 5) = [1, 2]
   * 
   * Appending extra zeros at the last part of the answer is allowed. 
   * For example, [1, 2], [1, 2, 0] and [1, 2, 0, 0] will be regarded as same answer.
   *
   * See https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Digit-by-digit_calculation for the algorithm.
   * 
   * e.g.
   *    for n = 2, 
   *    start 1st iteration with p = 0
   *    greatest integer x s.t. y = x*(20*p+x)<= 2 is 1 
   * 
   *    now 100 * (2-y) = 100 and new p = 1
   *    greatest integer x s.t. y = x*(20*p+x)<= 100 is 4 
   * 
   *    now 100 * (100-y) = 400 and new p = 14
   *    greatest integer x s.t. y = x*(20*p+x)<= 400 is 1 
   * 
   *    now 100 * (400-y) = 12000 and new p = 141
   *    greatest integer x s.t. y = x*(20*p+x)<= 12000 is 4 
   * 
   *    now 100 * (12000-y) = 720 and new p = 1414
   *    ...
   * 
   * e.g.
   *    for n = 1.44, 
   *    start 1st iteration with p = 0
   *    greatest integer x s.t. y = x*(20*p+x)<= 1 is 1 
   * 
   *    now 100 * (1-y) + 44 = 44 and new p = 1
   *    greatest integer x s.t. y = x*(20*p+x)<= 44 is 2
   * 
   *    now 100 * (44-y) = 0, so the iteration ends with new p = 12.
   *
   */

  def decSqrt(n: LazyList[Int], len: Int): LazyList[Int] = {
    // parameters
    // 1. p (0이 초기값, 지금 p에 10 곱해서 max x 더한 다음 loop으로 넘김)
    // 2. lim (맨 앞자리수가 초기값, 100 * (지금 lim - 지금 y) + 남은 두자리수) 더해서 다음으로 넘김
    // 3. len_now
    // 4. num_list
    // 5. result
    // inside
    // 1. x: 1~9까지, x(20*p + x) > lim인 순간 x-1을 x값으로 확정
    // 2. 그때 y값 계산하기
    // 4. 다음의 p, lim, result 계산
    // 5. 현재 list에서 두개 빼기 (없으면 INil)
    // 6. 다음 loop로 넘기기

    def calY(x: Int, p: Int): Int = {
      x * (20 * p + x)
    }

    @tailrec def findX(x: Int, p: Int, lim: Int): Int = {
      if (calY(x, p) > lim) x - 1
      else if (x == 9) x
      else findX(x + 1, p, lim)
    }

    def findNextP(p: Int, lim: Int): Int = {
      10 * p + findX(0, p, lim)
    }


    def findNextLim(lim: Int, y: Int, num_list: LazyList[Int]): Int = {
      num_list.get match {
        case None => 100 * (lim - y)
        case Some(hd, tl) =>
          tl.get match {
            case None => 100 * (lim - y) + 10 * hd
            case Some(hd2, tl2) => 100 * (lim - y) + 10 * hd + hd2
          }
      }
    }



    def eliminateN(num_list: LazyList[Int], count: Int): LazyList[Int] = {
      @tailrec def eliminate(list_now: LazyList[Int], count_now: Int): LazyList[Int] = {
        if (count_now == count) list_now
        else {
          list_now.get match {
            case None => LNil()
            case Some(hd, tl) =>
              eliminate(tl, count_now+1)
          }
        }
      }
      eliminate(num_list, 0)
    }


    @tailrec def dec(len_now: Int, num_list: LazyList[Int], result: LazyList[Int], p: Int, lim: Int): LazyList[Int] = {
      printf("p: %d, lim: %d\n", p, lim)
      if (len < len_now) result
      else {
        val x = findX(0, p, lim)
        val y = calY(x, p)
        val next_p = 10 * p + x
        val next_lim = findNextLim(lim, y, num_list)
        val next_result = result.append(LCons(x, LNil()))
        dec(len_now + 1, eliminateN(num_list, 2), next_result, next_p, next_lim)
      }

    }

    n.get match {
      case None => LNil()
      case Some(h, t) => dec(0, t, LNil(), 0, h)
    }

  }
