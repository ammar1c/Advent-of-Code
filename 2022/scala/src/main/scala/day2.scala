import day1.getClass

object day2:

  enum Shape(val score: Int):
    case Rock extends Shape(1)
    case Paper extends Shape(2)
    case Scissors extends Shape(3)

  enum Outcome(val score: Int):
    case Lose extends Outcome(0)
    case Win extends Outcome(6)
    case Draw extends Outcome(3)

  object Outcome:

    import Shape._

    def parse(c: Char): Either[Throwable, Outcome] = c match
      case 'X' => Right(Lose)
      case 'Y' => Right(Draw)
      case 'Z' => Right(Win)
      case _ => Left(IllegalArgumentException("Unrecognized character"))


    def chooseWinner(first: Shape, second: Shape): Outcome =
      if first == second then Draw
      else if (first == Rock && second == Scissors ||
        first == Paper && second == Rock ||
        first == Scissors && second == Paper) then Lose
      else Win


  object Shape:

    import Outcome._

    val winnerLoser = List(
      Paper -> Rock,
      Rock -> Scissors,
      Scissors -> Paper
    )

    def pickShape(shape: Shape, outcome: Outcome): Shape =
      import Outcome._
      outcome match
        case Lose => winnerLoser.find(_._1 == shape).map(_._2).get
        case Draw => shape
        case Win => winnerLoser.find(_._2 == shape).map(_._1).get


    def parse(c: Char): Either[Throwable, Shape] = c match
      case 'A' | 'X' => Right(Rock)
      case 'B' | 'Y' => Right(Paper)
      case 'C' | 'Z' => Right(Scissors)
      case _ => Left(IllegalArgumentException("Unrecognized shape"))


  def main(args: Array[String]): Unit =
    val file = getClass.getResource("/day2.txt").getFile
    val lines = io.Source.fromFile(file).getLines().toList
    import Shape._
    val first =
      lines.flatMap { line =>
        val Array(first, second) = line.split(" ").map(_.head)
        parse(first).flatMap(s1 => parse(second).map(s2 => (s1, s2))).toOption
      }
        .map { case (s1, s2) =>
          val winner = Outcome.chooseWinner(s1, s2)
          winner.score + s2.score
        }.sum
    println(first)
    val second =
      lines.flatMap { line =>
        val Array(first, second) = line.split(" ").map(_.head)
        parse(first).flatMap(s1 => Outcome.parse(second).map(s2 => (s1, s2))).toOption
      }.map { case (shape, outcome) =>
        pickShape(shape, outcome).score + outcome.score
      }.sum
    println(second)

