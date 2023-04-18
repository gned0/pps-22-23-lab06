package u06lab.code

object Solitaire extends App:

  type Position = (Int, Int)
  type Solution = Iterable[Position]
  type IterableFactory = Solution => Iterable[Solution]
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placeMarks(width: Int, height: Int)(using factory: IterableFactory): Iterable[Solution] = (width, height) match
    case (1, 1) => factory(Set((0, 0)))
    case _ =>
      ???



  println(render(solution = Seq((0, 0), (2, 1)), width = 5, height = 5))