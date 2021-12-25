package aoc2021

object day25 {

  def main(args: Array[String]): Unit = {
    val input  = read("input")
    println(input.map(_.toSeq).toSeq)
    println(part1(input))
  }

  def part1(input: Array[Array[Char]]): Int = {
    var (m, n) = (input.length, input(0).length)

    var cp = input.map(_.toArray).toArray
    var hasMoved = false
    for(i <- 0 until m; j <- 0 until n) {
      if(input(i)(j) == '>' && input(i)((j + 1) % n) == '.') {
        cp(i)(j) = '.'
        cp(i)((j + 1) % n) = '>'
        hasMoved = true
      }
    }

    var cp1 = cp.map(_.toArray).toArray
    for(i <- 0 until m; j <- 0 until n) {
      if(cp(i)(j) == 'v' && cp((i+1)%m)(j) == '.') {
        cp1(i)(j) = '.'
        cp1((i+1)%m)(j) = 'v'
        hasMoved = true
      }
    }
    if (hasMoved) return 1 + part1(cp1) else return 1
  }

  def read(input: String): Array[Array[Char]] = {
    val lines = scala.io.Source.fromFile(s"./data/day25-$input.txt").getLines.toArray
    lines.map(_.trim.toCharArray).toArray
  }
}
