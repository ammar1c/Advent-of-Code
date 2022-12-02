import scala.collection.mutable

object day1 {

  def main(args: Array[String]): Unit = {
    val file = getClass.getResource("/day1.txt").getFile
    val lines = io.Source.fromFile(file).getLines()
    val sums = mutable.ArrayDeque.empty[Long]
    for(line <- lines) {
      if(sums.isEmpty) sums += 0

      if (line.isEmpty) sums += 0
      else  sums.update(sums.size-1, sums.last+line.toLong)
    }
    println(sums.max)
    println(sums.sortBy(-_).take(3).sum)

  }
}
