package aoc2021

object day15 {

  def solve(grid: Array[Array[Int]], p: Int): Int = {
    // solve it using dijkstra
    val (om, on) = (grid.length, grid(0).length)
    val (m, n) = (om*p, on*p)

    val ngrid = Array.tabulate(m, n) { (i, j) =>
      var v = grid(i % om)(j % on)
      v = (v + i / om - 1) % 9 + 1
      v = (v + j / on - 1) % 9 + 1
      v
    }
    type Node = (Int, (Int, Int))
    implicit val ordering: Ordering[Node] = (x: Node, y: Node) => x._1.compare(y._1)
    val pq = scala.collection.mutable.PriorityQueue.empty[Node](ordering.reverse)
    pq.enqueue((0, (0, 0)))
    val dist = scala.collection.mutable.ArraySeq.fill(m, n)(Int.MaxValue)
    dist(0)(0) = 0
    val visited = scala.collection.mutable.Set.empty[(Int, Int)]
    while (pq.nonEmpty) {

      val (d, (i,j)) = pq.dequeue()
      if (!visited((i,j))) {
        visited.add((i,j))
        for ((di, dj) <- Seq((-1, 0), (1, 0), (0, -1), (0, 1))) {
          val (ni, nj) = (i + di, j + dj)
          if (visited((ni,nj)) || ni < 0 || ni >= m || nj < 0 || nj >= n) {}
          else {
            dist(ni)(nj) = math.min(dist(ni)(nj), d + ngrid(ni)(nj))
            pq.enqueue((dist(ni)(nj), (ni, nj)))
          }
        }
      }
    }
    return dist(m-1)(n-1)
  }


  def main(args: Array[String]): Unit = {
    val grid = scala.io.Source.fromFile("./data/day15-input.txt")
                              .getLines
                              .map(_.map(_ - '0').toArray).toArray
    println(solve(grid, 1))
    println(solve(grid, 5))
  }
}
