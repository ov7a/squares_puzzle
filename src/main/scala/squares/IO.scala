package squares

import squares.solver.Solution

import java.io.{InputStream, OutputStream, PrintWriter}
import scala.io.{Source, StdIn}


object IO:
  def parseSquares(input: InputStream): IndexedSeq[Square] =
    Source.fromInputStream(input)
      .getLines()
      .filter(_.nonEmpty)
      .map(parseSquare)
      .toIndexedSeq

  private def parseSquare(line: String): Square =
    val parts = line.split(" ").map(_.toInt)
    require(parts.length == 4, "Invalid input: line must have 4 elements")
    Square(parts(0), parts(1), parts(2), parts(3))

  def print(solutions: Iterator[Solution], outStream: OutputStream): Unit =
    val writer = new PrintWriter(outStream)
    solutions.foreach { solution =>
      writer.write(toString(solution))
      writer.write("\n\n")
    }
    writer.flush()

  private def toString(solution: Solution): String =
    solution
      .map(_.productIterator.mkString(" "))
      .mkString("\n")

  def parseBoard(input: InputStream): Board =
    val table = Source.fromInputStream(input)
      .getLines()
      .map(_.split(" ").map(_.toInt))
      .toArray

    Board(table)

