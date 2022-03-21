package squares

case class Board(table: Array[Array[Int]]) {

  private val maxRow = table.length
  private val maxColumn = table.head.length

  val cellsCount: Int = table.flatten.count(_ != 0)

  val topLeftCorners: Map[(Int, Int), Corner] =
    (0 to maxRow).flatMap { row =>
      (0 to maxColumn).flatMap { column =>
        getTopLeftCorner(row, column).map((row, column) -> _)
      }
    }.toMap

  private val cellsLocations: Map[Int, (Int, Int)] =
    (0 until maxRow).flatMap { row =>
      (0 until maxColumn).flatMap { column =>
        getCellIndex(row, column).map(_ -> (row, column))
      }
    }.toMap

  private def getTopLeftCorner(row: Int, column: Int): Option[Corner] =
    val elements = Seq(
      getCellIndex(row, column).map(CornerElement(_, _.topLeft)),
      getCellIndex(row - 1, column).map(CornerElement(_, _.bottomLeft)),
      getCellIndex(row - 1, column - 1).map(CornerElement(_, _.bottomRight)),
      getCellIndex(row, column - 1).map(CornerElement(_, _.topRight)),
    ).flatten

    elements.size match
      case 4 => Some(Corner.Full(elements))
      case 3 | 2 => Some(Corner.Partial(elements))
      case _ => None

  private def getCellIndex(row: Int, column: Int): Option[Int] =
    table.lift(row).flatMap(_.lift(column)).filter(_ > 0)

  def getCorners(cellIndex: Int): Iterator[Corner] =
    val (row, column) = cellsLocations(cellIndex)
    Iterator(
      topLeftCorners.get((row, column)),
      topLeftCorners.get((row + 1, column)),
      topLeftCorners.get((row + 1, column + 1)),
      topLeftCorners.get((row, column + 1)),
    ).flatten
}

enum Corner(val elements: Seq[CornerElement]):
  case Full(override val elements: Seq[CornerElement]) extends Corner(elements)
  case Partial(override val elements: Seq[CornerElement]) extends Corner(elements)

case class CornerElement(cellIndex: Int, numberGetter: Square => Int)
