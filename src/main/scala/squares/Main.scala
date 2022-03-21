package squares

import squares.solver.*

import java.io.{File, FileInputStream, InputStream, OutputStream}
import scala.io.{Source, StdIn}
import scala.util.Using


@main
def main =
  Using.resources(
    System.in,
    System.out
  ) { (in, out) =>
    solve(in, out, BacktrackSolver)
  }

def solve(input: InputStream, output: OutputStream, solver: Solver): Unit =
  val squares = IO.parseSquares(input)
  require(squares.size == defaultBoard.cellsCount, s"Input should have ${defaultBoard.cellsCount} squares")

  val solutions = solver
    .solve(squares, defaultBoard, defaultSum)
    .distinct

  IO.print(solutions, output)


val defaultSum = 10

val defaultBoard = Board(Array(
  Array(0, 1, 2, 0),
  Array(3, 4, 5, 6),
  Array(7, 8, 9, 10),
  Array(0, 11, 12, 0),
))
