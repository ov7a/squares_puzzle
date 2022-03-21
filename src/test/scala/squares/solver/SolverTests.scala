package squares.solver

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.*
import squares.*

import java.io.{File, FileInputStream, InputStream}
import scala.io.Source
import scala.util.Using

trait SolverTests(solver: Solver) extends AnyFunSuite with should.Matchers :
  protected lazy val name: String = solver.getClass.getSimpleName

  testCases.foreach { testCase =>
    test(s"process ${testCase.name} correctly") {
      val solutions = solver.solve(testCase.input, testCase.board, testCase.maxSum).distinct
      solutions.toSet shouldBe testCase.output
    }
  }

  case class TestCase(name: String, input: IndexedSeq[Square], board: Board, maxSum: Int, output: Set[Solution])

  protected def testCases: Seq[TestCase] =
    val casesDir = File(getClass.getResource("/cases").toURI)

    casesDir
      .listFiles()
      .filter(_.isDirectory)
      .map { caseDir =>
        val input = process(caseDir, "squares.txt", IO.parseSquares)

        val boardFile = File(caseDir, "board.txt")
        val board = if (boardFile.exists())
          process(boardFile, IO.parseBoard)
        else
          defaultBoard

        val maxSum = readSum(File(caseDir, "sum.txt"))

        val output: Set[Solution] = process(caseDir, "solutions.txt", IO.parseSquares)
          .grouped(board.cellsCount)
          .toSet

        TestCase(
          name = caseDir.getName,
          input = input,
          board = board,
          maxSum = maxSum,
          output = output,
        )
      }

  private def process[T](dir: File, fileName: String, action: InputStream => T): T =
    process(File(dir, fileName), action)

  private def process[T](file: File, action: InputStream => T): T =
    Using.resource(FileInputStream(file.getPath))(action)

  private def readSum(file: File): Int =
    if (file.exists())
      Using.resource(Source.fromFile(file)) { source =>
        source.getLines().mkString.toInt
      }
    else
      defaultSum

