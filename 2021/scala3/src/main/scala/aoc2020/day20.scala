package aoc2020

import scala.annotation.tailrec

object day20 {
  case class Player(decks: List[Int])
  def parse(): (Player, Player) =
    val input = day20Data.input.split("\n\n")
    val deque1 = input(0).split("\n").drop(1).map(_.toInt).toList
    val deque2 = input(1).split("\n").drop(1).map(_.toInt).toList
    (Player(deque1), Player(deque2))


  @tailrec
  def part1(players: (Player, Player)): Long = players match
      case (Player(Nil), Player(d2)) => d2.reverse.zipWithIndex.foldLeft(0L)((acc, x) => acc + x._1 * (x._2+1))
      case (Player(d1), Player(Nil)) => d1.reverse.zipWithIndex.foldLeft(0L)((acc, x) => acc + x._1 * (x._2+1))
      case (Player(c1 :: tail1), Player(c2 :: tail2)) if c1 > c2 => part1(Player(tail1 :+ c1 :+ c2) -> Player(tail2))
      case (Player(c1 :: tail1), Player(c2 :: tail2)) => part1(Player(tail1) -> Player(tail2 :+ c2 :+ c1))


  def main(args: Array[String]): Unit = {
    println(part1(parse()))
  }


}

object day20Data:
  val input =
    """
      |Player 1:
      |18
      |19
      |16
      |11
      |47
      |38
      |6
      |27
      |9
      |22
      |15
      |42
      |3
      |4
      |21
      |41
      |14
      |8
      |23
      |30
      |40
      |13
      |35
      |46
      |50
      |
      |Player 2:
      |39
      |1
      |29
      |20
      |45
      |43
      |12
      |2
      |37
      |33
      |49
      |32
      |10
      |26
      |36
      |17
      |34
      |44
      |25
      |28
      |24
      |5
      |48
      |31
      |7
      |""".stripMargin.trim
