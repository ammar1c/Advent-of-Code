package aoc

object day19 {

  final case class Point(x: Int, y: Int, z: Int):
    override def toString: String = s"($x, $y, $z)"

    def -(that: Point): Point = Point(this.x - that.x, this.y - that.y, this.z - that.z)

    def +(that: Point): Point = Point(this.x + that.x, this.y + that.y, this.z + that.z)


  final case class Scanner(beacons: Set[Point], pos: Option[Point]):
    def ++(that: Scanner): Scanner = Scanner((this.beacons ++ that.beacons), this.pos)

    def adjustToNewOrigin(norigin: Point): Scanner = this.copy(pos = Some(norigin), beacons = beacons.map(p => norigin + p))

    private def ordering: (Int, Point) => Point = (i, p) => List(
      Point(p.x, p.y, p.z),
      Point(p.x, p.z, p.y),
      Point(p.y, p.x, p.z),
      Point(p.y, p.z, p.x),
      Point(p.z, p.x, p.y),
      Point(p.z, p.y, p.x)
    )(i)

    def allCombinations: Seq[Scanner] =
      val ors = Seq(-1, 1)

      for x1 <- ors; y1 <- ors; z1 <- ors; i <- 0 to 5
        yield
          this.copy(beacons = this.beacons.map(p => {
            val np = ordering(i, p)
            np.copy(x = np.x * x1, y = np.y * y1, z = np.z * z1)
          }))


  def detectOverlapping(scn0: Scanner, scn1: Scanner, threshold: Int): Option[Point] =
    var result = (None: Option[Point], List.empty[Point])
    for p0 <- scn0.beacons; p1 <- scn1.beacons do
        val scn1PredPos = p0 - p1
        var overlap = List.empty[Point]
        for px <- scn1.beacons do
          val p0pred = scn1PredPos + px
          if scn0.beacons.contains(p0pred) then
            overlap = overlap :+ p0pred
        if overlap.size >= threshold && result._1.size < overlap.size then
          return Some(scn1PredPos)
    None


  def read(path: String): List[Scanner] =
    val text = scala.io.Source.fromFile(path)
    val lines = text.getLines().toList
    var id = 0
    var scanners = List.empty[Scanner]
    var scanner: Option[Scanner] = None
    lines.foreach { line =>
      if line.trim.isBlank then
        if id == 0 then scanner = scanner.map(sc => sc.copy(pos = Some(Point(0, 0, 0))))
        scanners = scanners ++ scanner
        id += 1
      else if line.contains("scanner") then
        scanner = Some(Scanner(Set.empty, None))
      else
        val Array(x, y, z) = line.split(",").map(_.toInt)
        scanner = scanner.map(sc => sc.copy(beacons = sc.beacons + Point(x, y, z)))
    }
    scanners = scanners ++ scanner
    return scanners


  def main(args: Array[String]): Unit =
    val scanners = read("./data/day19-sample.txt")


    var distance = 0

    var visitedx = Set.empty[Int]
    var positions = Map.empty[Int, Point]

    def dfs(last: Scanner, start: Int): Scanner =
      for (i <- start until scanners.length) {
        if (!visitedx.contains(i)) {
          for (scn <- scanners(i).allCombinations) {
            val pos = detectOverlapping(last, scn, 12)
            if (pos.nonEmpty) {
              positions = positions.updated(i, pos.get)
              visitedx = visitedx + i
              val nlast = scn.adjustToNewOrigin(pos.get)
              val comp = last ++ nlast
              return dfs(comp, 1)
            }
          }
        }
      }
      return last


    val sca = dfs(scanners(0), 1)
    println(s"part1: ${sca.beacons.size}")

    val abs = math.abs
    val n = scanners.length
    positions = positions.updated(0, Point(0, 0, 0))
    val mxDis = (0 until scanners.length).zip(1 until scanners.length).map { xy =>
      val p0 = positions(xy._1)
      val p1 = positions(xy._2)
      abs(p0.x - p1.x) + abs(p0.y - p1.y) + abs(p0.z - p1.z)
    }.max


    println(s"part2: ${mxDis}")

}
