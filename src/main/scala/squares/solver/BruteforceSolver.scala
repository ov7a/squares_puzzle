package squares.solver

import squares.solver.Solution
import squares.{Board, Corner, Square}

object BruteforceSolver extends Solver :
  override def solve(input: IndexedSeq[Square], board: Board, maxSum: Int): Iterator[Solution] =
    input
      .combinations(board.cellsCount)
      .flatMap(_.permutations)
      .filter(isValid(_, board, maxSum))

  private def isValid(squares: IndexedSeq[Square], board: Board, maxSum: Int): Boolean =
    board.topLeftCorners.values.forall(cornerIsValid(_, maxSum, squares.lift))
