package aoc2021


object day23 {
  enum SpaceType(val s: Char, val energy: Int):
    case Empty extends SpaceType('.', 0)
    case A extends SpaceType('A', 1)
    case B extends SpaceType('B', 10)
    case C extends SpaceType('C', 100)
    case D extends SpaceType('D', 1000)
    override def toString: String = s.toString

  object SpaceType:
    def apply(c: Char): SpaceType = c match
      case '.' => Empty
      case 'A' => A
      case 'B' => B
      case 'C' => C
      case 'D' => D

//  def play(game: Game): Long = {
//    if (game.rooms.forall(a => a._1 == a._2.head && a._2.forall(_ == a._1))) then 0L
//    else if (game.immediatelyOutsideAnyRoom) then Long.MaxValue
//    else {
//      // pick one and move it in the hallway
//
//    }
//
//  }

  case class Game(hall: Seq[SpaceType],
                  rooms: Map[SpaceType, Seq[SpaceType]],
                  roomIdx : Map[SpaceType, Int],
                  idxToRoom: Map[Int, SpaceType]) {
    def immediatelyOutsideAnyRoom: Boolean =
      hall.zipWithIndex.forall(a => a._1 == SpaceType.Empty || !idxToRoom.contains(a._2))

    def moveToHall: List[(Int, Game)] = {
      var picked = SpaceType.Empty
      var result = List.empty[(Int, Game)]

      for((roomType, values) <- rooms) {
        if (values(0) != roomType && values(0) != SpaceType.Empty) {
          var newValues = values
          val save = values(0)
          newValues = newValues.updated(0, SpaceType.Empty)
          val pos = roomIdx(roomType)
          var canMove = true
          var moves = 2
          // go left
          for(left <- pos-1 to 0 by -1) {
            if (hall(left) != SpaceType.Empty) canMove = false
            if (canMove && !idxToRoom.contains(left)) {
              val g1 = this.copy(
                hall = hall.updated(left, save),
                rooms = rooms.updated(roomType, newValues),
              )
              result = result :+ ((moves * save.energy, g1))
            }
            moves += 1
          }
          // go right
          canMove = true
          moves = 2
          for(right <- pos+1 until hall.length) {
            if (hall(right) != SpaceType.Empty) canMove = false
            if (canMove && !idxToRoom.contains(right)) {
              val g1 = this.copy(
                hall = hall.updated(right, save),
                rooms = rooms.updated(roomType, newValues),
              )
              result = result :+ ((moves * save.energy, g1))
            }
            moves += 1
          }
        }
      }
      result
    }

    def idToSpace(id: Int): Seq[SpaceType] = rooms(idxToRoom(id))

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
    var indices = List.empty[Int]
    var hall = List.empty[SpaceType]
    var rooms = Map.empty[SpaceType, Array[SpaceType]]
    var idxToRoom = Map.empty[Int, SpaceType]
    var roomToIdx = Map.empty[SpaceType, Int]
    for((c, i) <- lines(0).zipWithIndex) {
      if c == '.' then hall :+= SpaceType(c)
      if lines(1)(i) != '#' then indices :+= (i-1)
    }
    val roomsSeq = Array(SpaceType.A, SpaceType.B, SpaceType.C, SpaceType.D)
    for((pos,roomType)  <- indices.zip(roomsSeq)) {
      var room = Array.fill(2)(SpaceType.Empty)
      for(x <- 1 to 2) room(x-1) = SpaceType(lines(x)(pos+1))
      rooms = rooms.updated(roomType, room)
      roomToIdx = roomToIdx.updated(roomType, pos)
      idxToRoom = idxToRoom.updated(pos, roomType)
    }
    Game(hall.toSeq, rooms.mapValues(_.toSeq).toMap, roomToIdx, idxToRoom)
  }

  def main(args: Array[String]): Unit = {
    val game = read("data/day23-sample.txt")
    for((e,g1) <- game.moveToHall) {
      println("energy " + e)
      println(g1)
    }
  }

}
