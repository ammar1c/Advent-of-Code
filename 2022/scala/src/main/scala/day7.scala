import day6.getClass

object day7 {
  trait CMD
  case class CD(input: String) extends CMD
  case class LS(output: Seq[Output]) extends CMD

  trait Output
  case class Dir(name: String) extends Output
  case class File(name: String, size: Int) extends Output

  import scala.collection.{mutable => mu}
  trait FSNode
  case class FNode(filename: String, size: Int) extends FSNode
  case class DirNode(files: mu.Map[String, FSNode] = mu.Map.empty, parent: Option[DirNode] = None) extends FSNode {
    def toStringT(n: Int = 0): String = {
      val tabs = "\t"*n
      files.map {
        case (name, FNode(filename, size)) => s"$tabs $name $size"
        case (name, z @ DirNode(files, _)) => s"$tabs $name \n ${z.toStringT(n+1)}"
      }.mkString("\n")

    }
    override def toString: String = toStringT(0)
  }

  def collect(path: String, node: FSNode, dirSize: mu.Map[String, Int]): Int = node match {
    case FNode(_, size) => size
    case DirNode(files, _) =>
      val absPath = path
      val size =  files.map {case (name, file) => collect(path + name + "/", file, dirSize)}.sum
      dirSize(path) = size
      dirSize(path)
  }

  def main(args: Array[String]): Unit = {
    val file = getClass.getResource("/day7.txt").getFile
    val source = io.Source.fromFile(file).getLines().toSeq
    val commands = (source :+ "$ ls").foldLeft((None: Option[CMD], Seq.empty[CMD])){
      case ((prev @ Some(LS(output)), cmds), line) if line.startsWith("dir") =>

        (prev.map(_.asInstanceOf[LS].copy(output = output :+ Dir(line.split(" ")(1)))), cmds)
      case ((prev @ Some(LS(output)), cmds), line) if !line.startsWith("$") =>
        val Array(filesize, filename) = line.split(" ")
        (prev.map(_.asInstanceOf[LS].copy(output = output :+ File(filename, filesize.toInt))), cmds)
      case ((prev, cmds), line) if line.startsWith("$") =>
        val tokens = line.split(" ")
        ( if (tokens(1) == "cd") Some(CD(tokens(2)))
        else Some(LS(Seq.empty)), cmds ++ prev)
    }._2

    val root = DirNode()
    commands.foldLeft(root) {
      case (c @ DirNode(subs, _), CD("..")) =>
        c.parent.getOrElse(c)
      case (c @ DirNode(subs, _), CD("/")) =>
        root.files("/") = root.files.get("/").getOrElse(DirNode()).asInstanceOf[DirNode]
        root.files("/").asInstanceOf[DirNode]
      case (c @ DirNode(subs, _), CD(dir)) =>
        subs(dir) = subs.get(dir).getOrElse(DirNode(parent = Some(c)))
        subs(dir).asInstanceOf[DirNode]
      case (c @ DirNode(subs, _), LS(output)) =>
        output.foreach {
          case Dir(name) =>
            subs(name) = subs.get(name).getOrElse(DirNode(parent = Some(c)))
          case File(name, size) =>
            subs(name) = subs.get(name).getOrElse(FNode(name, size))
        }
        c
    }
    val m = mu.Map.empty[String, Int]
    val total = collect("/", root.files("/"), m)
    val needSpace = 30000000 - (70000000 - total)

    println(total)
    println(needSpace)
    println(m.filter(_._2 <= 100000).map(_._2).sum)
    println(m.toSeq.sortBy(_._2).find(_._2 >= needSpace))

  }
}
