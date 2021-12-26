package aoc2020
import scala.io.Source.fromFile
object day6 {

  def part1(input: String): Long = {
      input.split("\n\n")
        .map(answers =>
          answers.split("\\s+").flatMap(_.toCharArray).toSet.size)
        .sum
  }

  def part2(input: String): Long = {
    input.split("\n\n")
        .map(answers => answers.split("\\s+"))
        .map{ group =>
          group.flatMap(_.toCharArray)
                .groupBy(identity)
                .mapValues(_.size)
                .filter(_._2 == group.size)
                .size
        }.sum
  }

  def main(args: Array[String]): Unit = {
    val input = fromFile("data/2020/day6.txt").mkString
    println("Part1: " + part1(input))
    println("Part2: " + part2(input))
  }
}
