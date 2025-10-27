package pp202402.midterm3

import scala.annotation.tailrec
import scala.util.control.TailCalls._

/** Principles of Programming: Midterm problem 3.
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
  * We do not require tail-recursion explicitly for the midterm except problem 2.
  *
  * Timeout: 30 sec.
  */
object Midterm3:
  /** Problem 3: Arithmetic Operation (40 Points)
    * 
    * Implement arithmetic operations with parametric polymorphism.
    * 
    * First, implement arithmetic operations of specific types: Int, Double, and Rational.
    * (Rational is a type of rational numbers represented as pairs of Int).
    * 
    * Second, implement polymorphic operations which have operations of specific types as inputs.
    */

  /** Problem 3-1: arithmetic operations of Int (5 Points)
   * Implement arithmetic operations of Int with two exceptions : 
   * DividedByZeroException (for divInt(x,0)) and
   * NotAnIntegerException (for divInt(x,y) where x%y != 0).
   */
  def addInt(x: Int, y: Int): Int = {
    x + y
  }
  def subInt(x: Int, y: Int): Int = {
    x - y
  }
  def mulInt(x: Int, y: Int): Int = {
    x * y
  }
  def divInt(x: Int, y: Int): Int = {
    if (y == 0) throw new DividedByZeroException("")
    else if (x % y != 0) throw new NotAnIntegerException("")
    else x / y
  }

  /** Problem 3-2: arithmetic operations of Double (5 Points)
   * Implement arithmetic operations of Double with DividedByZeroException (for divDouble(x,0)).
   */
  def addDouble(x: Double, y: Double): Double = {
    x + y
  }
  def subDouble(x: Double, y: Double): Double = {
    x - y
  }
  def mulDouble(x: Double, y: Double): Double = {
    x * y
  }
  def divDouble(x: Double, y: Double): Double = {
    if (y == 0) throw new DividedByZeroException("")
    else x / y
  }

  /** Problem 3-3: arithmetic operations of rational number (5 Points)
   * 
   * Rational numbers are represented as a pair of Int. (See Data.scala.)
   * 
   * For example, 3/5 is represented as (3,5).
   * 
   * Implement arithmetic operations of Rational with DividedByZeroException.
   * 
   * Note that you should simplify the result in irreducible fraction form : 
   * if the computed value is (x,y), then y>0 and the only common divisor of |x| and |y| is 1 (in natural number).
   * 
   * Input will be given as same form, too. 
   */
  def abs(x: Int): Int = {
    if (x < 0) -x
    else x
  }


  def gcd(x: Int, y: Int): Int = {
    @tailrec def gcdIn(a: Int, b: Int): Int = {
      if (b == 0) a
      else gcdIn(b, a % b)
    }
    if (x > y) gcdIn(x, y)
    else gcdIn(y, x)
  }


  def addRational(x: Rational, y: Rational): Rational = {
    (x, y) match {
      case ((a, 0), (c, d)) => throw new DividedByZeroException("")
      case ((a, b), (c, 0)) => throw new DividedByZeroException("")
      case ((a, b), (c, d)) =>
        ((a*d + b*c) / gcd(abs(a*d + b*c), abs(b*d)), b*d / gcd(abs(a*d + b*c), abs(b*d)))
    }
  }
  def subRational(x: Rational, y: Rational): Rational = {
    (x, y) match {
      case ((a, 0), (c, d)) => throw new DividedByZeroException("")
      case ((a, b), (c, 0)) => throw new DividedByZeroException("")
      case ((a, b), (c, d)) =>
        ((a*d - b*c) / gcd(abs(a*d - b*c), abs(b*d)), b*d / gcd(abs(a*d - b*c), abs(b*d)))
    }
  }
  def mulRational(x: Rational, y: Rational): Rational = {
    (x, y) match {
      case ((a, 0), (c, d)) => throw new DividedByZeroException("")
      case ((a, b), (c, 0)) => throw new DividedByZeroException("")
      case ((a, b), (c, d)) =>
        ((a*c) / gcd(abs(a*c), abs(b*d)), (b*d) / gcd(abs(a*c), abs(b*d)))
    }
  }
  def divRational(x: Rational, y: Rational): Rational = {
    (x, y) match {
      case ((a, 0), (c, d)) => throw new DividedByZeroException("")
      case ((a, b), (0, d)) => throw new DividedByZeroException("")
      case ((a, b), (c, d)) =>
        ((a*d) / gcd(abs(a*d), abs(b*c)), (b*c) / gcd(abs(a*d), abs(b*c)))
    }
  }

  /** Problem 3-4: polymorphic arithmetic evaluation (20 Points)
    * 
    * Implement polymorphic arithmetic evaluation of Expr (in Data.scala) with arithmetic functions as inputs.
    */
  import Result.*
  import Expr.*
  import Error.*

  def eval[T](e: Expr[T], addF: (T, T) => T, subF: (T, T) => T, mulF: (T, T) => T, divF: (T, T) => T): Result[T] = {

    def evalIn(eI: Expr[T]): T = {
      eI match {
        case Num(n) => n
        case Add(e1, e2) => addF(evalIn(e1), evalIn(e2))
        case Sub(e1, e2) => subF(evalIn(e1), evalIn(e2))
        case Mul(e1, e2) => mulF(evalIn(e1), evalIn(e2))
        case Div(e1, e2) => divF(evalIn(e1), evalIn(e2))
      }
    }

    try {
      Success(evalIn(e))
    } catch {
      case e: DividedByZeroException => Failure(DividedByZero)
      case e: NotAnIntegerException => Failure(NotAnInteger)
    }
  }