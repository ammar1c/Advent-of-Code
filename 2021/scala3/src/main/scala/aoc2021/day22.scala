package aoc2021

import scala.collection.{mutable => mu}

object day22 {

  enum State:
    case Off
    case On
  object State {
    def and(a: State, b: State): State =
      if (a == b) a else Off
  }

  case class Range(start: Int, end: Int, inc: Boolean = true) {
    def size: Int =
      if (inc) end - start + 1 else end - start
  }

  case class Cube(state: State, x: Range, y: Range, z: Range) {
    def intersects(other: Cube): Option[Cube] = getOverlap(this, other)
    def count: Int = x.size * y.size * z.size
    def tryMerge(other: Cube) = inside(this, other).orElse(inside(other, this))
    def split(overlap: Cube): List[Cube] = {
      val cube_x1 = this.copy(x = Range(x.start, overlap.x.start, inc = false))
      val cube_x2 = this.copy(x = Range(overlap.x.end, this.x.end, inc = false))
      val cube_y1 = this.copy(y = Range(y.start, overlap.y.start, inc = false), x = Range(overlap.x.start, overlap.x.end))
      val cube_y2 = this.copy(y = Range(overlap.y.end, y.end, inc = false), x = Range(overlap.x.start, overlap.x.end))
      val cube_z1 = this.copy(z = Range(z.start, overlap.z.start, inc = false), x = Range(overlap.x.start, overlap.x.end), y = Range(overlap.y.start, overlap.y.end))
      val cube_z2 = this.copy(z = Range(overlap.z.end, z.end, inc = false), x = Range(overlap.x.start, overlap.x.end), y = Range(overlap.y.start, overlap.y.end))
      return List(cube_x1, cube_x2, cube_y1, cube_y2, cube_z1, cube_z2)
    }

    override def toString: String = s"($state, $x, $y, $z) ${count}"
  }



  def main(args: Array[String]): Unit = {
    val cubes = readCubes("")
    var result = apply(cubes)

    println(result)
  }

  def inside(big: Cube, small: Cube): Option[Cube] ={
    if (small.x.start >= big.x.start && small.x.end <= big.x.end &&
        small.y.start >= big.y.start && small.y.end <= big.y.end &&
        small.z.start >= big.z.start && small.z.end <= big.z.end)
      Some(big)
    else
      None
  }

  def readCubes(path: String): mu.ArrayDeque[Cube] = {
    val lines = scala.io.Source.fromFile("./data/day22-sample.txt").getLines
    val reg = "-?\\d+".r
    val cubes = mu.ArrayDeque.empty[Cube]
    for(line <- lines) {
      val ranges = reg.findAllIn(line).toList.map(_.toInt)
      val state = if line.startsWith("on") then State.On else State.Off
      cubes += Cube(state = state, x = Range(ranges(0), ranges(1)), y = Range(ranges(2), ranges(3)), z = Range(ranges(4), ranges(5)))
    }
    cubes
  }

  def apply(cubes: mu.ArrayDeque[Cube]): Int = {
    var result = 0
    var onCubes = mu.ArrayDeque.empty[Cube]
    for(cube <- cubes) {

      val sz = onCubes.size
      for(i <- 0 until sz) {
        val onCube = onCubes.removeHead()
        val overlap = cube.intersects(onCube)
        if (overlap.exists(_.count > 0)) {
          val newCubes = onCube.split(overlap.get)
          println(s"result of ${onCube} \n ${cube} is ")
          println(s"overlap = ${overlap}")
          println(s"new cubes =")
          for(newCube <- newCubes) {

              println(newCube)
              onCubes += newCube

          }
          if (cube.state == State.On) {
            onCubes += overlap.get
          }
        } else {
          onCubes += onCube
        }
      }
      if (cube.state == State.On) {
        onCubes += cube
      }
    }
    println(onCubes.mkString("\n"))
    for (cube <- onCubes) {
      result += cube.count
    }
    return result
//    for(i <- cubes.indices) {
//      for(j <- 0 until cubes.length if i != j) {
//        val cube1 = cubes(i)
//        val cube2 = cubes(j)
//        val cube = cube1.intersects(cube2)
//        println(s"$i: $cube1")
//        println(s"$j: $cube2")
//        println(s"intersctiono: $cube ${cube.map(_.count)}")
//      }
//    }
    return 0
  }


  def getOverlap(cube1: Cube, cube2: Cube): Option[Cube] = {
    for(x <- Seq(cube1.x.start, cube1.x.end) ; y <- Seq(cube1.y.start, cube1.y.end); z <- Seq(cube1.z.start, cube1.z.end)) {
      if (cube2.x.start < x && x < cube2.x.end &&
        cube2.y.start < y && y < cube2.y.end &&
        cube2.z.start < z && y < cube2.z.end) then
        return Some(Cube(state = cube1.state,
          x = Range(math.max(cube1.x.start, cube2.x.start), math.min(cube1.x.end, cube2.x.end)),
          y = Range(math.max(cube1.y.start, cube2.y.start), math.min(cube1.y.end, cube2.y.end)),
          z = Range(math.max(cube1.z.start, cube2.z.start), math.min(cube1.z.end, cube2.z.end))))
    }
    None
  }

  def getOverlap1(cube1: Cube, cube2: Cube): Option[Cube] = {
    if (cube2.x.start > cube1.x.start && cube2.x.start < cube2.x.end) {
      if (cube2.y.start > cube1.y.start && cube2.y.start < cube2.y.end) {
        if (cube2.z.start > cube1.z.start && cube2.z.start < cube2.z.end) {
          val xRange = Range(math.max(cube1.x.start, cube2.x.start), math.min(cube1.x.end, cube2.x.end))
          val yRange = Range(math.max(cube1.y.start, cube2.y.start), math.min(cube1.y.end, cube2.y.end))
          val zRange = Range(math.max(cube1.z.start, cube2.z.start), math.min(cube1.z.end, cube2.z.end))
          return Some(Cube(state = State.and(cube1.state,cube2.state), x = xRange, y = yRange, z = zRange))
        }
      }
    }
    None
  }




}
