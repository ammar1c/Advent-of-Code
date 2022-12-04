import day3.getClass

import scala.annotation.tailrec

object day4:

  type Range = (Int, Int)

  def parseRange(rangeStr: String): Range =
    val Array(f, s) = rangeStr.split("-")
    (f.toInt, s.toInt)


  def fullyContains(r1: Range, r2: Range) =
    r1._1 <= r2._1 && r1._2 >= r2._2

  @tailrec
  def overlap(r1: Range, r2: Range): Boolean =
    if (r1._1 > r2._1) overlap(r2, r1)
    else r2._1 <= r1._2


  def main(args: Array[String]): Unit =
    val file = getClass.getResource("/day4.txt").getFile
    val lines: Seq[(Range, Range)] =
        io.Source.fromFile(file).getLines()
          .map { line =>
            val Array(first, second) = line.split(",")
            (parseRange(first), parseRange(second))
          }
          .toList

    val first = lines.count { case (r1, r2) =>
      fullyContains(r1, r2) || fullyContains(r2, r1)
    }
    println(first)
    val second = lines.count(overlap.tupled)
    println(second)


