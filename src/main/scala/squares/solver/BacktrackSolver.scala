package squares.solver

import squares.solver.Solution
import squares.{Board, Corner, Square}

object BacktrackSolver extends Solver :
  override def solve(input: IndexedSeq[Square], board: Board, maxSum: Int): Iterator[Solution] =
    def step(current: Map[Int, Square], notUsed: Set[Int], cellIndex: Int): Iterator[Solution] =
      notUsed.iterator.flatMap { candidateIndex =>
        val updated = current.updated(cellIndex - 1, input(candidateIndex))
        val valid = board.getCorners(cellIndex).forall(cornerIsValid(_, maxSum, updated.get))
        if (valid)
          if (cellIndex < board.cellsCount)
            step(updated, notUsed - candidateIndex, cellIndex + 1)
          else
            Iterator(updated.toSeq.sortBy(_._1).map(_._2))
        else
          Iterator.empty
      }

    step(
      current = Map.empty,
      notUsed = input.indices.toSet,
      cellIndex = 1
    )



