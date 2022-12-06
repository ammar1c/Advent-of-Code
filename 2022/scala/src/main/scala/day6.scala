import day4.getClass

object day6 {
  def main(args: Array[String]): Unit = {
    val file = getClass.getResource("/day6.txt").getFile
    val source = io.Source.fromFile(file).mkString

    def findFirstStart(source: String, idx: Int, window: String, size: Int): Int = {
      if (window.size < size) findFirstStart(source, idx+1, window + source(idx), size)
      else if (window.toSet.size == window.size) idx
      else findFirstStart(source, idx+1, window.substring(1) + source(idx), size)
    }
    println(findFirstStart(source, 0, "",4 ))
    println(findFirstStart(source, 0, "", 14))

  }
}
