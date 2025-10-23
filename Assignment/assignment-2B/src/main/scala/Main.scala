package pp202502.assign2b

import scala.annotation.tailrec
import scala.util.control.TailCalls._

/** Principles of Programming: Assignment 2B.
  *
  * Implement given functions, which are currently left blank. (???) **WARNING:
  * Please read the restrictions below carefully.**
  *
  * If you do not follow these, **your submission will not be graded.**
  *
  *   - Do not use the keyword `var`. Use `val` and `def` instead.
  *   - Do not use any library functions or data structures like `List`,
  *     `Range` (`1 to n`, `1 until n` ...), `fold`, `map`, `reduce` or
  *     etc except for `Array`.
  *   - If you want to use a data structure, create new one instead of using the
  *     library ones.
  *   - You can only use tuples, `scala.annotation.tailrec`, and
  *     `scala.util.control.TailCalls._`, `Array` from the library.
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
object Assignment2B:
  import IOption.*

  /** Problem 3: Sudoku Solver
   * 
   * Solve a given 9x9 Sudoku puzzle. A Sudoku board is a 9x9 grid,
   * where some cells are pre-filled with numbers between 1 and 9, and other cells 
   * are empty (represented by 0). The goal is to fill all the empty cells with
   * numbers between 1 and 9 while satisfying the following conditions:
   * 
   * 1. Each number from 1 to 9 must appear exactly once in each row.
   * 2. Each number from 1 to 9 must appear exactly once in each column.
   * 3. Each number from 1 to 9 must appear exactly once in each of the nine 3x3 subgrids.
   * 
   * This means that for any empty cell, you need to find a valid number such that 
   * it does not already exist in its corresponding row, column, or subgrid. 
   * Check the link below for more information on Sudoku puzzles:
   * https://en.wikipedia.org/wiki/Sudoku
   * 
   * The function `solveSudoku(board: Array[Int]): IOption[Array[Int]]` 
   * takes a 9x9 Sudoku board as input and returns an `IOption` of the solved board.
   * If the board is solvable, the function should return `ISome(solvedBoard)`.
   * If the board is not solvable, the function should return `INone`.
   * 
   * Note: While you are not allowed to use `Array` in other problems, 
   * we have provided a board as a 1D array using `Array` here. You can use indexing to access elements in the board.
   * You can think of this 1D array as a flattened version of a 2D array, where each row is concatenated sequentially.
   * You can access to the value of an array with the following syntax: 
   * `A(3)` for the 3rd element of array A.
   */
   def solveSudoku(board: Array[Int]): IOption[Array[Int]] = {
    val N = 9
    def convertIndex(row: Int, col: Int): Int = row * N + col
    def getElement(board: Array[Int], row: Int, col: Int): Int = 
      board(convertIndex(row, col))
    def updateElement(board: Array[Int], row: Int, col: Int, num: Int): Unit = 
      board(convertIndex(row, col)) = num

    def checkFormation(index: Int, numInput: Int): Boolean = {
      val row = index / 9
      val col = index - row * 9
      @tailrec def checkCol(rowVal: Int): Boolean = {
        if (rowVal > 8) true
        else if (rowVal != row && getElement(board, rowVal, col) == numInput) false
        else checkCol(rowVal+1)
      }
      @tailrec def checkRow(colVal: Int): Boolean = {
        if (colVal > 8) true
        else if (colVal != col && getElement(board, row, colVal) == numInput) false
        else checkRow(colVal+1)
      }
      @tailrec def checkGrid(rowVal: Int, colVal: Int): Boolean = {
        if ((rowVal, colVal) != (row, col) && getElement(board, rowVal, colVal) == numInput) false
        else if ((rowVal+1) % 3 == 0 && (colVal+1) % 3 == 0) true
        else if ((colVal+1) % 3 == 0) checkGrid(rowVal+1, colVal-2)
        else checkGrid(rowVal, colVal+1)
      }
      checkCol(0) && checkRow(0) && checkGrid(row/3*3, col/3*3)
    }


     def backTrack(index: Int, numInput: Int): Boolean = {

       if (index > 80) true
       else if (numInput == 1 && board(index) != 0) {
         if (checkFormation(index, board(index))) backTrack(index + 1, 1)
         else false
       }
       else if (numInput == 10) {
         board(index) = 0
         false
       }
       else if (checkFormation(index, numInput)) {
         board(index) = numInput
         if (backTrack(index + 1, 1)) true
         else backTrack(index, numInput + 1)
       }
       else {
         backTrack(index, numInput + 1)
       }

     }

     if (backTrack(0, 1)) ISome(board)
     else INone

   }