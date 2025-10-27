package pp202402.midterm2

import scala.annotation.tailrec
import scala.util.control.TailCalls._

/** Principles of Programming: Midterm problem 2.
  *
  * Implement given functions, which are currently left blank. (???) **WARNING:
  * Please read the restrictions below carefully.**
  *
  * If you do not follow these, **your submission will not be graded.**
  *
  *   - Do not use the keyword `var`. Use `val` and `def` instead.
  *   - Do not use any library functions or data structures like `List`,
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
  * Timeout: 30 sec.
  */
object Midterm2:
  import IList.*

  /** Problem 2: Tail Recursion (30 Points)
    * 
    * Implement functions using tail recursion.  
    */
    
  /** Problem 2-1: Reverse in List (10 Points)
    * 
    * Implement a reverse function of IList using tail recursion. 
    * 
    * For example, reverse(ICons(1, ICons(2, ICons(3, INil)))) = ICons(3, ICons(2, ICons(1, INil)))
    */
  def reverseTail[A](x: IList[A]): IList[A] = {
    @tailrec def reverse(x1: IList[A], result: IList[A]): IList[A] = {
      x1 match {
        case INil => result
        case ICons(h, t) => reverse(t, ICons(h, result))
      }
    }
    reverse(x, INil)
  }
      
  /** Problem 2-2: Fold in List (20 Points)
    * 
    * Implement a fold function of IList (that of problem 1-1) with tail recursion. 
    */ 
  def foldListTail[A,B](x: B, f: (A,B) => B): IList[A] => B = {
    def foldList(l1: IList[A]): B = {
      @tailrec def fold(l2: IList[A], result: B): B = {
        l2 match {
          case INil => result
          case ICons(h, t) => fold(t, f(h, result))
        }
      }
      fold(reverseTail(l1), x)
    }
    foldList _
  }

  