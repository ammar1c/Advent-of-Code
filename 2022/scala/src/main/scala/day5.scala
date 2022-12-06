import day6.getClass
import scala.collection.{mutable=>mu}
object day5 {

  def main(args: Array[String]): Unit = {
    val file = getClass.getResource("/day5.txt").getFile
    val source = io.Source.fromFile(file).getLines().toSeq
    val idx = source.zipWithIndex.find {
      case (line, idx) =>
        line.strip().startsWith("1")
    }.get._2
    val pattern = "[0-9]+".r
    val boxes = pattern.findAllMatchIn(source(idx)).map(math => math.start).toSeq
    val stacks = (0 to boxes.size).map(idx => scala.collection.mutable.ArrayDeque.empty[Char])
    source.slice(0, idx).foreach { line =>
      for((boxIdx,boxId) <- boxes.zipWithIndex) {
        if (line.size > boxIdx && line(boxIdx) != ' ') stacks(boxId+1).prepend(line(boxIdx))
      }
    }
    val stacks1 =  mu.ArrayDeque.from(stacks.map(mu.ArrayDeque.from))
    val stacks2 =  mu.ArrayDeque.from(stacks.map(mu.ArrayDeque.from))

    for(i <- idx+2 until source.size) {
      val line = source(i)
      val Array(count, from, to) = pattern.findAllIn(line).map(_.toInt).toArray
      stacks2(to) ++= stacks2(from).takeRight(count)
      stacks2(from).dropRightInPlace(count)
      for(_ <- 0 until count)
        stacks1(to).append(stacks1(from).removeLast())

    }
    println(stacks1.slice(1, stacks1.size).flatMap(_.lastOption).mkString)
    println(stacks2.slice(1, stacks2.size).flatMap(_.lastOption).mkString)
  }
}
