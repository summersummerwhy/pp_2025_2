package pp202402.assign3b

import scala.reflect.ClassTag
import pp202402.assign3b.Data._
import pp202402.assign3b.ViewController._
import scala.annotation.tailrec

object Assignment3B:
  /** Principles of Programming: Assignment 3B.
   * This assignment is about implementing a simple game engine that can be used to play two games: Omok and Sudoku.
   * 
   * The game engine is implemented using the State design pattern, where the state of the game is represented by `State`.
   * - `State`: contains the current state of the game board and the current player.
   * The game engine also uses the Rule design pattern, where the rules of the game are implemented in `Rule`.
   * - `Rule`: contains the rules of the game, such as the initial message, initial value, and the rules for making a move.
   * The game engine also uses the ViewController design pattern, where the view of the game is implemented in a `ViewController`.
   * - `ViewController`: contains the methods to display the game board and the values on the board.
   * The game engine also uses the GameController class to control the game.
   * - `GameController`: contains the methods to initialize the game, display the game board, and make a move.
   * 
   * The game engine is implemented in a generic way, so it can be used to play any game that 
   * can be represented by the `State`, `Rule`, and `ViewController` classes.
   * Check `UI.scala` to understand how the game engine is used to play Omok and Sudoku.
   * 
   * Fill in the TODOs
   * - in the `OmokState` and `SudokuState` classes to implement the state of the Omok and Sudoku games.
   * - in the `OmokRule` and `SudokuRule` classes to implement the rules of the Omok and Sudoku games.
   * - in the `GameController` class to implement the game controller.
   * 
   * You are free to implement more functions inside the classes if needed.
   * 
   * There are no TODOs on other files, but you can check them to understand the game engine.
   * We will only extract your `Main.scala` to grade your assignment.
   * Thus, please do not make changes on other files.
   */
  
  /** Omok game.
   * 
   * Omok is a two-player game where the players take turns to fill the board with Os and Xs.
   * - The game board is a 5x5 board.
   * - A player wins if they have 5 consecutive pieces in a row, column, or diagonal.
   * - Player 1 ALWAYS makes the first move when the game starts.
   * - Also, players will take turns to fill the board until one player wins or the board is full without a winner.
   */

  /** Omok state.
   * Here, you should implement the state of the Omok game using the `OmokState` class that extends the `State` class.
   * There are two states in the Omok game:
   * - a board with "O"s, "X"s, and empty cells (" "). Each cell is represented by a string value.
   * - the current player.
   *
   * Also, there are two methods in the `OmokState` class:
   * - `fillCell`: fills the cell at the given row and column with the given value.
   * - `nextTurn`: returns the state that represents the next turn. 
   *    If the current player is Player1, the next player is Player2. If the current player is Player2, the next player is Player1.
   * Here, note that we have splitted the stage of updating the board and the stage of updating the player, but
   * a consistent game state requires the all the updates to be done at one turn.
   */
  class OmokState(board: MyArray[String], currentPlayer: Player) extends State[String](board, currentPlayer, 5, 5):
    // TODOs start here //
    def fillCell(i: Int, j: Int, v: String): OmokState = {
      // fills the cell at the given row and column with the given value
      if i < 0 || i >= 5 || j < 0 || j >= 5 then
        throw BoardOutOfBoundsException("Board out of bounds")
      val board_updated = board.update(i * 5 + j, v)
      new OmokState(board_updated, currentPlayer)
    }

    def nextTurn(): OmokState = {
      currentPlayer.match {
        case Player.Player1 => new OmokState(board, Player.Player2)
        case Player.Player2 => new OmokState(board, Player.Player1)
      }
    }
      
  /** Omok rule.
   * 
   * Here, you should implement the rules of the Omok game using the `OmokRule` class that extends the `Rule` class.
   * We will introduce rules that we are going to check in public/hidden tests.
   * We will not check the rules that are not introduced here.
   */
  class OmokRule extends Rule[String]:
    def initMessage(): String =
      "Omok: Fill the board with Os and Xs. A player wins if they have 5 consecutive pieces in a row, column, or diagonal.\n" +
      "Player 1 is O and Player 2 is X. Player 1 starts first.\n"
    
    def initValue(): String = " "
    def player1: String = "O"
    def player2: String = "X"
  
    // TODOs start here //

    // TODO: Implement the renderInput method for the Omok game.
    // Render a input character into a string value. A valid input should be either 'O' or 'X'.
    // If the input is not valid, throw an InvalidInputException.
    def renderInput(input: Char): String = {
      input match {
        case 'O' | 'X' => input.toString
        case _ => throw InvalidInputException("input not O X")
      }
    }
    
    // TODO: Implement the isAvailableValue method for the Omok game.
    // - check1 if the value is valid. A valid value should be either "O" or "X".
    def isAvailableValue(v: String): Boolean = {
      v match {
        case "O" | "X" => true
        case _ => false
      }
    }

    // TODO: Implement the isStateValid method for the Omok game.
    // Given a current state with a board and the current player, 
    // check if the state is consistent
    // - hint: think about how many pieces each player can have on the board when they take turns.
    def isStateValid(state: State[String]): Boolean = {
      // 참고할 수 있는 것: getPlayer, getNRows, getNCols, getCell, nextTurn
      // player 1: O수, X수 같아야함
      // player 2: O수가 X수보다 하나 커야함

      @tailrec def checkPieces(row: Int, col: Int, p1_num: Int, p2_num: Int): Boolean = {
        if (row == state.getNRows()) {
          state.getPlayer() match {
            case Player.Player1 => {
              if (p1_num == p2_num) true
              else false
            }
            case Player.Player2 => {
              if (p1_num == p2_num + 1) true
              else false
            }
          }
        }
        else if (col == state.getNCols()) checkPieces(row + 1, 0, p1_num, p2_num)
        else {
          state.getCell(row, col) match {
            case "O" => checkPieces(row, col + 1, p1_num + 1, p2_num)
            case "X" => checkPieces(row, col + 1, p1_num, p2_num + 1)
            case _ => checkPieces(row, col + 1, p1_num, p2_num)
          }
        }
      }

      checkPieces(0, 0, 0, 0)
    }

    // TODO: Implement the isNextMoveValid method for the Omok game.
    // Given a current state with a board, the current player, 
    // and the next move (i, j, v) this player wants to make,
    // check if the next move is valid.
    // - check1: you cannot put a piece on a cell that is already filled.
    // - check2: you cannot put a piece on a cell if the game is already done.
    def isNextMoveValid(state: State[String], i: Int, j: Int, v: String): Boolean = {
      val inBounds =
        0 <= i && i < state.getNRows() &&
          0 <= j && j < state.getNCols()

      val empty = state.getCell(i, j) == initValue()

      val correctTurn =
        state.getPlayer() match
          case Player.Player1 => v == player1  // O 차례면 O만
          case Player.Player2 => v == player2  // X 차례면 X만

      inBounds && empty && !isDone(state) && correctTurn
    }

    // TODO: Implement the isDone method for the Omok game.
    // Given a current state with a board, check if the game is done.
    // The game is done if one player wins or the board is full.
    def isDone(state: State[String]): Boolean = {
      // check rows
      def checkRow(): Boolean = {
        @tailrec def checkRowI(row: Int, col: Int, consec: Int, consec_type: String): Boolean = {
          if (consec == 5) true
          else if (row == 5) false
          else if (col == 5) checkRowI(row + 1, 0, 0, initValue())
          else {
            val now_type = state.getCell(row, col)
            if (now_type != initValue() && now_type == consec_type) checkRowI(row, col + 1, consec + 1, consec_type)
            else checkRowI(row, col + 1, 1, now_type)
          }
        }
        checkRowI(0, 0, 0, initValue())
      }
      // check cols
      def checkCol(): Boolean = {
        @tailrec def checkColI(row: Int, col: Int, consec: Int, consec_type: String): Boolean = {
          if (consec == 5) true
          else if (col == 5) false
          else if (row == 5) checkColI(0, col + 1, 0, initValue())
          else {
            val now_type = state.getCell(row, col)
            if (now_type != initValue() && now_type == consec_type) checkColI(row + 1, col, consec + 1, consec_type)
            else checkColI(row + 1, col, 1, now_type)
          }
        }

        checkColI(0, 0, 0, initValue())
      }
      // check diagonals
      def checkDiag(): Boolean = {
        @tailrec def checkDownRight(i: Int, gap: Int, consec: Int, consec_type: String): Boolean = {
          if (consec == 5) true
          else if (i == 5) false
          else {
            val now_type = state.getCell(i, i + gap)
            if (now_type != initValue() && now_type == consec_type) checkDownRight(i + 1, gap, consec + 1, consec_type)
            else checkDownRight(i + 1, gap, 1, now_type)
          }
        }

        @tailrec def checkUpRight(i: Int, sum: Int, consec: Int, consec_type: String): Boolean = {
          if (consec == 5) true
          else if (i == 5) false
          else {
            val now_type = state.getCell(i, sum - i)
            if (now_type != initValue() && now_type == consec_type) checkUpRight(i + 1, sum, consec + 1, consec_type)
            else checkUpRight(i + 1, sum, 1, now_type)
          }
        }

        checkDownRight(0, 0, 0, initValue()) || checkUpRight(0, 4, 0, initValue())
      }

      @tailrec def checkFull(row: Int, col: Int): Boolean = {
        if (row == 5) true
        else if (col == 5) checkFull(row + 1, 0)
        else if (state.getCell(row, col) == initValue()) false
        else checkFull(row, col + 1)
      }

      checkRow() || checkCol() || checkDiag() || checkFull(0, 0)
    }

  /** Sudoku game.
   * 
   * Sudoku is a single-player game where the player fills the board with numbers from 1 to 9.
   * - The game board is a 9x9 board.
   * - No number can be repeated in the same row, column, or 3x3 subgrid.
   * - The game is done when the board is full and the rules are satisfied.
   * - The player do not have to check if the initial board is can be solved or not.
   */

  /** Sudoku state.
   * Here, you should implement the state of the Sudoku game using the `SudokuState` class that extends the `State` class.
   * There are two states in the Sudoku game:
    * - a board with numbers from 1 to 9 and empty cells. Each cell is represented by a `SudokuCell` value.
    * - the current player.
    * 
    * Also, there are two methods in the `SudokuState` class:
    * - `fillCell`: fills the cell at the given row and column with the given value.
    * - `nextTurn`: returns the state that represents the next turn.
    */
  class SudokuState(board: MyArray[SudokuCell], currentPlayer: Player) extends State[SudokuCell](board, currentPlayer, 9, 9):
    def fillCell(i: Int, j: Int, v: SudokuCell): SudokuState = ???

    def nextTurn(): SudokuState = ???

  /** Sudoku rule.
   * 
   * Here, you should implement the rules of the Sudoku game using the `SudokuRule` class that extends the `Rule` class.
   * We will introduce rules that we are going to check in public/hidden tests.
   * We will not check the rules that are not introduced here.
   */
  class SudokuRule extends Rule[SudokuCell]:
    def initMessage(): String = "Sudoku: Fill the board with numbers from 1 to 9.\n" +
      "No number can be repeated in the same row, column, or 3x3 subgrid.\n"
    def initValue(): SudokuCell = SudokuCell.Empty
    
    // TODOs start here

    // TODO: Implement the renderInput method for the Sudoku game.
    // Render a input character into a SudokuCell value. A valid input should be a character from '1' to '9'.
    // If the input is not valid, throw an InvalidInputException.
    // You can assume that only player input will be passed to this method. (No fixed values)
    def renderInput(input: Char): SudokuCell = ???

    // TODO: Implement the isAvailableValue method for the Sudoku game.
    // Determine if the input SudokuCell is available to put on the board.
    // - check1: a player is allowed to put a number from 1 to 9.
    // - check2: a player is allowed to put an empty cell.
    def isAvailableValue(v: SudokuCell): Boolean = ???
    
    // TODO: Implement the isStateValid method for the Sudoku game.
    // Given a current state with a board and the current player,
    // check if the state is consistent
    // - check1: No number can be repeated in the same row, column, or 3x3 subgrid.
    def isStateValid(state: State[SudokuCell]): Boolean = ???

    // TODO: Implement the isNextMoveValid method for the Sudoku game.
    // Given a current state with a board, the current player,
    // and the next move (i, j, v) this player wants to make,
    // check if the next move is valid.
    // - check1: you cannot put a number on a cell that is already fixed.
    // - check2: you cannot put a number on a cell if the game is already done.
    def isNextMoveValid(state: State[SudokuCell], i: Int, j: Int, v: SudokuCell): Boolean = ???
    
    // TODO: Implement the isDone method for the Sudoku game.
    // Given a current state with a board, check if the game is done.
    // - check1: The game is done if the board is full and the rules are satisfied.
    def isDone(state: State[SudokuCell]): Boolean = ???

  /** Game controller.
   * Implement the game controller using the `GameController` class.
   */ 
  class GameController[A: ClassTag](rule: Rule[A], state: State[A], viewController: ViewController[A]):
    def init(): String = rule.initMessage()

    def display(): String = viewController.displayBoardState(state)
        
    def isDone(): Boolean = rule.isDone(state)

    // TODO starts here //

    // TODO: Implement the makeMove method for the game controller.
    // Given the row, column, and value (Char) the player wants to put on the board,
    // check if the move is valid and update the game state.
    // If the move is not valid, throw an InvalidMoveException.
    def makeMove(i: Int, j: Int, v: Char): GameController[A] = {
      if (rule.isStateValid(state) && rule.isNextMoveValid(state, i, j, rule.renderInput(v))) {
        val updatedState = state.fillCell(i, j, rule.renderInput(v))
        new GameController[A](rule, updatedState.nextTurn(), viewController)
      }
      else throw InvalidMoveException("Invalid Move!")
    }
