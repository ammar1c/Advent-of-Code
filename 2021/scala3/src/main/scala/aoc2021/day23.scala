package aoc2021

import scala.collection.mutable.{Map, ArrayDeque}

object day23 {
  enum SpaceType(val s: Char):
    case Empty extends SpaceType('.')
    case A extends SpaceType('A')
    case B extends SpaceType('B')
    case C extends SpaceType('C')
    case D extends SpaceType('D')
    override def toString: String = s.toString

  object SpaceType:
    def apply(c: Char): SpaceType = c match
      case '.' => Empty
      case 'A' => A
      case 'B' => B
      case 'C' => C
      case 'D' => D


  case class Game(hall: Array[SpaceType],
                  rooms: Map[SpaceType, Array[SpaceType]],
                  idxToRoom: Map[Int, SpaceType]) {
    def idToSpace(id: Int): Array[SpaceType] = rooms(idxToRoom(id))

    override def toString: String = {
      val len = hall.length+2
      val sb = new StringBuilder
      sb.append("#"*len)
      sb.append("\n")
      sb.append("#")
      hall.foreach(sb.append(_))
      sb.append("#")
      sb.append("\n")
      for(i <- 0 until 2) {
        for (j <- 0 until len) {
          if (idxToRoom.contains(j-1))
            sb.append(idToSpace(j-1)(i))
          else
            sb.append("#")
        }
        sb.append("\n")
      }
      sb.append("#"*len)
      sb.append("\n")
      sb.toString()
    }
  }

  def read(file: String): Game = {
    val lines = scala.io.Source.fromFile(file).getLines.toArray.drop(1)
    var indices = ArrayDeque.empty[Int]
    var hall = ArrayDeque.empty[SpaceType]
    var rooms = Map.empty[SpaceType, Array[SpaceType]]
    val idxToRoom = Map.empty[Int, SpaceType]
    for((c, i) <- lines(0).zipWithIndex) {
      if c == '.' then hall += SpaceType(c)
      if lines(1)(i) != '#' then indices += (i-1)
    }
    val roomsSeq = Array(SpaceType.A, SpaceType.B, SpaceType.C, SpaceType.D)
    for((pos,roomType)  <- indices.zip(roomsSeq)) {
      var room = Array.fill(2)(SpaceType.Empty)
      for(x <- 1 to 2) room(x-1) = SpaceType(lines(x)(pos+1))
      rooms += (roomType -> room)
      idxToRoom += (pos -> roomType)
    }
    Game(hall.toArray, rooms, idxToRoom)
  }

  def main(args: Array[String]): Unit = {
    val game = read("data/day23-input.txt")
    println(game)
  }

}
