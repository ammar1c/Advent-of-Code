package aoc2020

object day13 {


  def part1(earliest: Long, buses: Seq[Long]): Long = {
    var result = Long.MaxValue
    var busId = 0
    for(bus <- buses) {
      if (bus != -1) {
        var loc = if (earliest % bus > 0) bus - earliest % bus else 0
        if (loc < result) {
          result = loc
          busId = bus.toInt
        }
      }
    }
    return result * busId
  }

  def part2(buses: Seq[Long]): Long = {
    var prod = 1L
    var x = 0L
    for((bus, i) <- buses.zipWithIndex) {

      if (bus != -1) {
        while((x+i) % bus != 0) x += prod
        prod *= bus
      }
    }
    return x
  }


  def parse() = {
    val arrivalTime = 1000510
    val buses = "19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,523,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,x,x,x,x,x,x,x,x,x,x,29,x,853,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23"
    (arrivalTime, buses.split(",").map(df => if (df == "x") -1 else df.toLong))
  }

  def main(args: Array[String]): Unit = {
    val (arrivalTime, buses) = parse()
    println(part1(arrivalTime, buses))
    println(part2(buses))
  }
}
