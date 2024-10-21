package u06lab.code

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  val solutions = placeMarks(5, 7)
  println(render(solution = solutions.head, width = 5, height = 7))
  // next line takes roughly 5 minutes to run
  // println("Number of found solutions (expected 13272): " + solutions.size)

type Position = (Int, Int)
type Solution = Seq[Position] // preferring more abstract DS

def placeMarks(width: Int, height: Int): Iterable[Solution] =
  def placeMarks(n: Int): Iterable[Solution] = n match
    case 1 => Seq(List((width/2, height/2)))
    case _ =>
      for
        solution <- placeMarks(n - 1)
        i <- 0 until width
        j <- 0 until height
        position = (i, j)
        if isValidMove(position, solution.head)
        if !(solution contains position) // infix notation
      yield position +: solution
  placeMarks(width * height)

def isValidMove(from: Position, to: Position): Boolean =
  val verticalOrHorizontalMove =
    (from._1 == to._1 && (from._2 - to._2).abs == 3) ||
      (from._2 == to._2 && (from._1 - to._1).abs == 3)
  val diagonalMove =
    (from._1 - to._1).abs == 2 &&
      (from._2 - to._2).abs == 2
  verticalOrHorizontalMove || diagonalMove