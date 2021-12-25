package aoc2021

import scala.collection.mutable.Map

object day20 {
  case class Board(b: Map[Int, Map[Int, Int]], default: Int) {
    lazy val m: Int = b.keys.max
    lazy val n: Int = b(0).keys.max
    def get(x: Int, y: Int): Int = b.getOrElse(x, Map.empty).getOrElse(y, default)
    def getZ(b: Map[Int, Map[Int, Int]], x: Int, y: Int): Int = b.getOrElse(x, Map.empty).getOrElse(y, default)
    def printBoard(): Unit =
      for x <- 0 to m do
        for y <- 0 to n do print(if (get(x, y) == 1) '#' else '.')
        println()

    var code = Seq.empty[Int]
    def neighbors(b: Map[Int, Map[Int, Int]], x: Int, y: Int): Int =
      val z = for dx <- -1 to 1
                  dy <- -1 to 1 yield getZ(b, x + dx, y + dy)
      val s = z.mkString("")
      val int = Integer.parseInt(s, 2)
      val code1 = code(int)
      println(x + " "  + y + " " + z  + " " + s + " " +int + " " + code1)
      Integer.parseInt(z.mkString, 2)

    def enhance(code: Seq[Int]): Board =  {
      this.code = code
      val newBoardX = Map.empty[Int, Map[Int, Int]]

      val newBoard = Map.empty[Int, Map[Int, Int]]

      for x <- 0 to m; y <- 0 to n do
        if !(newBoard contains x) then newBoard(x) = Map.empty[Int, Int]
        newBoard(x)(y) = code(neighbors(b,x,y))
      println("New board")
      Board(newBoard, default).printBoard()
      for x <- 0 to m+2 do newBoardX(x) = Map.empty[Int, Int]
      for x <- 0 to m; y <- 0 to n do newBoardX(x)(y) = newBoard(x)(y)

//      println("New Board x")
//      Board(newBoardX, default).printBoard()

      for y <- 0 to n+2 do newBoardX(0)(y) = code(neighbors(b, -1,y))
      for y <- 0 to n+2 do newBoardX(m+2)(y) = code(neighbors(b, m+1,y))
      for x <- 0 to m+2 do newBoardX(x)(0) = code(neighbors(b, x,-1))
      for x <- 0 to m+2 do newBoardX(0)(n+2) = code(neighbors(b, x,n+1))
      val newDefault = code(Integer.parseInt(s"$default"*9, 2))
      return Board(newBoardX, newDefault)
    }
  }
  def parse(path: String): (Seq[Int], Board) = {
    def toBin(c: Char): Int = if (c == '#') 1 else 0
    val Array(head, image) = io.Source.fromFile(path).mkString("").split("\n\n")
    val code = head.map(toBin).toSeq
    val lines = image.split("\n")
    val board = Map.empty[Int, Map[Int, Int]]
    for(i <- 0 until lines.length) {
      board(i) = Map.empty[Int, Int]

      lines(i).zipWithIndex.foreach {
        case (c, j) =>
          board(i)(j) = toBin(c)
      }
    }
    return (code, Board(board, 0))
  }

  def main(args: Array[String]): Unit = {
    val (code, board) = parse("./data/day20-sample.txt")

    board.printBoard()
    val newBoard = board.enhance(code)
    println()
    newBoard.printBoard()
  }

}
