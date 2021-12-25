package aoc2021

import scala.collection.{mutable => mu}

object day22 {

  import State._

  enum State:
    case Off
    case On
    def and(that: State): State = if (this == Off || that == Off) Off else On
    def not: State = if (this == On) Off else On


  case class Cube(state: State, x1: Int, x2: Int, y1: Int, y2: Int, z1: Int, z2: Int):
    def toggle: Cube = copy(state = state match {
      case Off => On
      case On => Off
    })
    def intersects(that: Cube): Boolean =
      !(x1 > that.x2 || x2 < that.x1 || y1 > that.y2 || y2 < that.y1 || z1 > that.z2 || z2 < that.z1)

    def intersection(that: Cube): Cube = Cube(this.state.not,
      x1 = math.max(x1, that.x1), x2 = math.min(x2, that.x2),
      y1 = math.max(y1, that.y1), y2 = math.min(y2, that.y2),
      z1 = math.max(z1, that.z1), z2 = math.min(z2, that.z2))
    def count(): Long = (x2 - x1 + 1L) * (y2 - y1 + 1L) * (z2 - z1 + 1L)


  def main(args: Array[String]): Unit = {
    val cubes = readCubes("")
    var seenCubes = List.empty[Cube]
    for(cube <- cubes) {
      for (seenCube <- seenCubes) {
        if (seenCube.intersects(cube)) {
          seenCubes = seenCubes :+ seenCube.intersection(cube)
        }
      }
      if cube.state == On then
        seenCubes = seenCubes :+ cube
    }
    println(seenCubes.foldLeft(0L)((acc, cube) => acc + (if cube.state == On then 1 else -1) * cube.count()))
  }

  def readCubes(path: String): mu.ArrayDeque[Cube] = {
    val lines = scala.io.Source.fromFile("./data/day22-input.txt").getLines
    val reg = "-?\\d+".r
    val cubes = mu.ArrayDeque.empty[Cube]
    for(line <- lines) {
      val ranges = reg.findAllIn(line).toList.map(_.toInt)
      val state = if (line.contains("on")) On else Off
      cubes += Cube(state = state, x1 = ranges.slice(0, 2).min, x2 = ranges.slice(0, 2).max,
              y1 = ranges.slice(2, 4).min, y2 = ranges.slice(2, 4).max,
        z1 = ranges.slice(4,6).min, z2 = ranges.slice(4, 6).max)
    }
    cubes
  }




}
