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

  val trace = true
  def playDijkstra(game: Game): Long = {
    var result = Long.MaxValue

    implicit val ordering: Ordering[(Long, Game)] = Ordering.by(-_._1)
    val pq = scala.collection.mutable.PriorityQueue[(Long, Game)]()
    pq.enqueue((0, game))

    val visited = scala.collection.mutable.Set.empty[Game]
    val trace = scala.collection.mutable.Map.empty[Game, Game]
    val energy = scala.collection.mutable.Map.empty[Game, Long]
    trace += game -> game

    while !pq.isEmpty do
      val (e, z) = pq.dequeue()
      energy += z -> e
//      println(s"${pq.size}")
      if (z.allOk) then {
//        println("Found result")
        var y = z
//        println(trace)
        var i = 0
        var l = scala.collection.mutable.Stack.empty[Game]
        while(trace(y) != y && i < 60) do
          l.push(y)
          y = trace(y)
          i += 1
        println(energy(y))
        println(y)
        while(l.nonEmpty)
          val g = l.pop()
          println(energy(g))
          println(g)



        return e
      }
      if (!visited(z)) {

        visited.add(z)
        val moves = z.moveToHall ++ z.moveFromHall
        for((e1, z1) <- moves) {
          if (!visited(z1)) {
            pq.enqueue((e + e1, z1))
            trace += z1 -> z
          }
        }

      }
    end while
    return 0
  }

  case class Game(hall: Seq[SpaceType],
                  rooms: Map[SpaceType, Seq[SpaceType]],
                  roomIdx : Map[SpaceType, Int],
                  idxToRoom: Map[Int, SpaceType]) {
    def toString1: String = s"Game(hall=$hall, rooms = $rooms, roomIdx = $roomIdx, idxToRoom = $idxToRoom)"

    def allOk: Boolean = {
      return hall.forall(_ == SpaceType.Empty) &&
        rooms.forall(x => x._2.forall(_ == x._1))
    }

    def immediatelyOutsideAnyRoom: Boolean = {
      return hall.zipWithIndex.forall(a => a._1 == SpaceType.Empty)
    }

    def moveFromHall: List[(Int, Game)] = {
      // grab an amphiod from the hall and move it into its room
      var result = List.empty[(Int, Game)]
      for((spaceType, pos) <- this.hall.zipWithIndex) {
        if (spaceType != SpaceType.Empty) {
            val idx = this.roomIdx(spaceType)
            var dstRoom = -1
            var canUpdate = true
            for(i <- 0 until roomSize) {
              if (rooms(spaceType)(i) == SpaceType.Empty && canUpdate) dstRoom += 1
              else canUpdate = false
            }
//            println(s"${dstRoom}")
            if (dstRoom >= 0
              && rooms(spaceType).drop(dstRoom+1).forall(_ == spaceType)){

              var moves = 1
              var dir = if idx < pos then -1 else 1
              var newPos = if idx < pos then pos-1 else pos+1

              while(newPos != idx && this.hall(newPos) == SpaceType.Empty) {
                newPos += dir
                moves += 1
              }

              if (!rooms(spaceType).drop(dstRoom+1).forall(_ == spaceType)) {
                println(s"Trying to move into ${rooms(spaceType)} value ${spaceType}")
              }


              if (newPos == idx) {
                result :+= ((moves + dstRoom + 1) * spaceType.energy, this.copy(
                  hall = hall.updated(pos, SpaceType.Empty),
                  rooms = rooms.updated(spaceType, rooms(spaceType).updated(dstRoom, spaceType))))

              }

            }
        }
      }
      return result
    }
    import scala.util.control._

    def moveToHall: List[(Int, Game)] = {
      var picked = SpaceType.Empty
      var result = List.empty[(Int, Game)]

      for((roomType, values) <- rooms) {

        var i = 0
        var loop = new Breaks
        loop.breakable {
          while (i < roomSize) {

            if (i > 0  && values(i-1) != SpaceType.Empty) loop.break // can't move this amph if theres no room
            if (i > 0 && (i until roomSize).forall(j => values(j) == roomType) && values(i) == roomType) loop.break
            if (i == 0 && (i until roomSize).forall(j => values(j) == roomType)) loop.break

//            println(i)
            if (values(i) == SpaceType.Empty) {
              // continue
            } else {

              var newValues = values
              val save = values(i)
              newValues = newValues.updated(i, SpaceType.Empty)

              val pos = roomIdx(roomType)
              var canMove = true
              var moves = 2 + i




              for(left <- pos-1 to 0 by -1) {
                if (hall(left) != SpaceType.Empty) canMove = false
                if (canMove && !idxToRoom.contains(left)) {
                  val g1 = this.copy(
                    hall = hall.updated(left, save),
                    rooms = rooms.updated(roomType, newValues))
                  if (g1 != this)  {
                    result = result :+ ((moves * save.energy, g1))
                  }
                }
                moves += 1
              }

              // go right
              canMove = true
              moves = 2 + i
              for(right <- pos+1 until hall.length) {
                if (hall(right) != SpaceType.Empty) canMove = false
                if (canMove && !idxToRoom.contains(right)) {
                  val g1 = this.copy(
                    hall = hall.updated(right, save),
                    rooms = rooms.updated(roomType, newValues))
                  if (g1 != this) {
                    result = result :+ ((moves * save.energy, g1))
                  }
                }
                moves += 1
              }

            }

            i += 1
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
      for(i <- 0 until roomSize) {
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
    val lines = scala.io.Source.fromFile(file).getLines
    parse(lines.toList)
  }

  var roomSize = 2

  def parse(linesIn: Seq[String]): Game = {
    var lines = linesIn.drop(1)
//    var indices = List.empty[Int]
    var hall = List.empty[SpaceType]
    var rooms = Map.empty[SpaceType, Array[SpaceType]]
    var idxToRoom = Map.empty[Int, SpaceType]
    var roomToIdx = Map.empty[SpaceType, Int]
    for((c, i) <- lines(0).zipWithIndex) {
      if c != '#' then hall :+= SpaceType(c)

    }
    val indices = lines(1).zipWithIndex.filter(_._1 != '#').map(_._2-1)
//    println("indices " + indices)
    val roomsSeq = Array(SpaceType.A, SpaceType.B, SpaceType.C, SpaceType.D)
    for((pos,roomType)  <- indices.zip(roomsSeq)) {
      var room = Array.fill(roomSize)(SpaceType.Empty)
      for(x <- 1 to roomSize) room(x-1) = SpaceType(lines(x)(pos+1))
      rooms = rooms.updated(roomType, room)
      roomToIdx = roomToIdx.updated(roomType, pos)
      idxToRoom = idxToRoom.updated(pos, roomType)
    }
    Game(hall.toSeq, rooms.mapValues(_.toSeq).toMap, roomToIdx, idxToRoom)
  }
  import scala.util.control._

  def move(game: Game, z: Int): Unit = {
    val moves = game.moveFromHall ++ game.moveToHall
    if (z >= 4) return
    if (moves.isEmpty) {
      println("No moves")
    } else {
      var i = 3
      val loop = new Breaks
      loop.breakable {
        for((e, g) <- moves) {
          println(s"$i $e")
          println("Moving from this")
          println(game)
          println("To this")
          println(g)
          move(g, z + 1)
          i -= 1
          if (i == 0) loop.break

        }
      }

    }
  }
  def part1() = {
    roomSize = 2
    val game = read("data/day23-input.txt")
    println(playDijkstra(game))
  }

  def part2() = {
    roomSize = 4
    val game = read("data/day23-input-p2.txt")
    println(playDijkstra(game))
  }

  def runTests(testF: String) = {
    val test = scala.io.Source.fromFile(testF).mkString.split("--\n")

    for (i <- 0 until test.length) yield

        println(test(i).split("\n").mkString("\n"))
        val game = parse(test(i).split("\n").toList)
        println("Before:")
        //      println(game.toString1)
        println(game)
        println("After move:")
          for ((e, g) <- game.moveFromHall ++ game.moveToHall)
            println(g)
        println("-------")
        println("Finish")


  }
  def main(args: Array[String]): Unit = {
      part2()
//    runTests()
//    move(game, 0 )
//    println(play(game))
//    val moves = game.moveToHall
//    println("Initially was")
//    println("Game")
//
//    for ((e, g) <- moves) {
//      println(s"$g")
//      for((e1, g1) <- g.moveFromHall) {
//        println("Moving from this:")
//        println(g)
//        println("To this:")
//        println(g1)
//      }
//
//    }

  }

}
