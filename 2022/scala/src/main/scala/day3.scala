import day2.getClass

object day3 {

  def score(c: Char): Int =
    if (c.isLower) c - 'a' + 1
    else c - 'A' + 27

  def main(args: Array[String]): Unit = {
    val file = getClass.getResource("/day3.txt").getFile
    val lines = io.Source.fromFile(file).getLines().toList
    val first = lines.map { line =>
      val (first, last) = line.splitAt(line.size / 2)
      first.toSet.intersect(last.toSet).head
    }.map(score).sum
    println(first)
    val second = lines.grouped(3).map { lines =>
      lines.map(_.toSet).reduce((a,b) => a.intersect(b)).head
    }.map(score).sum
    println(second)
  }
}
