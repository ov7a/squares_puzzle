package squares.solver

import squares.*

type Solution = Seq[Square]

trait Solver:
  def solve(squares: IndexedSeq[Square], board: Board, maxSum: Int): Iterator[Solution]
  
  def cornerIsValid(corner: Corner, maxSum: Int, getSquare: Int => Option[Square]): Boolean =
    val elements = corner.elements.flatMap { element =>
      getSquare(element.cellIndex - 1).map(element.numberGetter)
    }

    corner match
      case Corner.Full(_) if elements.size == 4 => elements.sum == maxSum
      case _ => elements.sum <= maxSum
