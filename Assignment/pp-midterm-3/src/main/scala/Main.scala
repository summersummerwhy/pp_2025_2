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
    (x, y) match {
      case (e1, 0) => throw new DividedByZeroException("")
      case (e1, e2) => {
        if (e1 % e2 != 0) throw new NotAnIntegerException("")
        else e1 / e2
      }
    }
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
    (x, y) match {
      case (e1, 0) => throw new DividedByZeroException("")
      case (e1, e2) => e1 / e2
    }
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

  def abs(a: Int): Int = {
    if (a < 0) -a
    else a
  }

  def gcd(a: Int, b: Int): Int = {
    val aPos = abs(a)
    val bPos = abs(b)
    def gcd_inside(dNow: Int, result: Int): Int = {
      if (dNow > aPos || dNow > bPos) result
      else {
        if (aPos % dNow == 0 && bPos % dNow == 0) gcd_inside(dNow+1, dNow)
        else gcd_inside(dNow+1, result)
      }
    }
    gcd_inside(1, 1)
  }
  def addRational(x: Rational, y: Rational): Rational = {
    (x, y) match {
      case ((a, 0), (c, d)) => throw new DividedByZeroException("")
      case ((a, b), (c, 0)) => throw new DividedByZeroException("")
      case ((a, b), (c, d)) => {
        val gcdCal = gcd((a*d + b*c), b*d)
        ((a*d + b*c)/gcdCal, (b*d)/gcdCal)
      }
      // 분자: a*d + b*c , 분모: b*d
    }
  }
  def subRational(x: Rational, y: Rational): Rational = {
    (x, y) match {
      case ((a, 0), (c, d)) => throw new DividedByZeroException("")
      case ((a, b), (c, 0)) => throw new DividedByZeroException("")
      case ((a, b), (c, d)) => {
        val gcdCal = gcd((a*d - b*c), b*d)
        ((a*d - b*c)/gcdCal, b*d/gcdCal)
      }
      // 분자: a*d - b*c , 분모: b*d
    }
  }
  def mulRational(x: Rational, y: Rational): Rational = {
    (x, y) match {
      case ((a, 0), (c, d)) => throw new DividedByZeroException("")
      case ((a, b), (c, 0)) => throw new DividedByZeroException("")
      case ((a, b), (c, d)) => {
        val gcdCal = gcd(a*c, b*d)
        (a*c / gcdCal, b*d / gcdCal)
      }
      // 분자: a*c , 분모: b*d
    }
  }
  def divRational(x: Rational, y: Rational): Rational = {
    (x, y) match {
      case ((a, 0), (c, d)) => throw new DividedByZeroException("")
      case ((a, b), (0, d)) => throw new DividedByZeroException("")
      case ((a, b), (c, d)) => {
        val gcdCal = gcd(a*d, b*c)
        (a*d / gcdCal, b*c / gcdCal)
      }
      // 분자: a*c , 분모: b*d
    }
  }

  /** Problem 3-4: polymorphic arithmetic evaluation (20 Points)
    * 
    * Implement polymorphic arithmetic evaluation of Expr (in Data.scala) with arithmetic functions as inputs.
    */
  def eval[T](e: Expr[T], addF: (T, T) => T, subF: (T, T) => T, mulF: (T, T) => T, divF: (T, T) => T): Result[T] = {
    import Expr.* // Num, Add, Sub, Mul, Div
    import Result.* // Success, Failure
    import Error.* // DividedByZero, NotAnInteger
    def eval_inside(ei: Expr[T]): T = {
      ei match {
        case Num(n) => n
        case Add(e1, e2) => addF(eval_inside(e1), eval_inside(e2))
        case Sub(e1, e2) => subF(eval_inside(e1), eval_inside(e2))
        case Mul(e1, e2) => mulF(eval_inside(e1), eval_inside(e2))
        case Div(e1, e2) => divF(eval_inside(e1), eval_inside(e2))
      }
    }

    try {
      Success(eval_inside(e))
    } catch {
      case e: DividedByZeroException => Failure(DividedByZero)
      case e: NotAnIntegerException => Failure(NotAnInteger)
    }
  }