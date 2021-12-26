package aoc2020
import scala.io.Source.fromFile
object day19 {

  sealed trait Rule
  final case class Terminal(c: Char) extends Rule
  final case class Sum(left: Product, right: Product) extends Rule
  final case class Product(rules: Reference*) extends Rule
  final case class Reference(rule: Int) extends Rule


  def parseRule(lines: Map[Int, String]): Map[Int, Rule] =
    import scala.collection.{mutable => mu}
    var rulesMemo = mu.Map.empty[Int, Rule]

    def parseProduct(rhs: List[String]): Product = {
      Product(rhs.map(x => Reference(x.toInt)): _*)
    }

    def parseRhs(line: Int, rhs: List[String]): Rule =
      if rhs(0).contains("\"") then Terminal(rhs(0).stripMargin('"').head)
      else if rhs.contains("|") then
        val idx = rhs.indexWhere(_.contains("|"))
        Sum(parseProduct(rhs.slice(0, idx)), parseProduct(rhs.slice(idx + 1, rhs.length)))
      else
        parseProduct(rhs)

    def parse(i: Int): Rule =
        if rulesMemo.contains(i) then rulesMemo(i)
        rulesMemo(i) = Reference(i) // for recursive rules
        val rhs = lines(i).split("\\s+").tail.toList
        rulesMemo += (i -> parseRhs(i, rhs))
        rulesMemo(i)

    for (i <- lines.keys) yield (i -> parse(i))
    return rulesMemo.toMap


  def matches(message: String, top: Rule, rules: Map[Int, Rule]): List[Int] =
    if message.isEmpty then Nil
    else top match
      case Terminal(c) => if message.head == c then List(1) else Nil
      case Sum(left, right) => matches(message, left, rules) ::: matches(message, right, rules)
      case Product(prodRules @ _*) => prodRules.foldLeft(List(0))((acc, rule) =>
        acc.flatMap(pos => matches(message.drop(pos), rule, rules).map(pos + _))
      )
      case Reference(i) => matches(message, rules(i), rules)

  def isValid(message: String, rule: Rule, rules: Map[Int, Rule]): Boolean = matches(message, rule, rules).exists(_ == message.length)



  def main(args: Array[String]): Unit =
    val Array(rulesIn, messages) = fromFile("./data/2020/day19.txt")
                                  .mkString
                                  .split("\n\n").map(_.split("\n").toSeq)

    val rules = rulesIn.map{ line =>
      val Array(left, right) = line.split(":")
      (left.toInt, line)
    }.toMap

    val generators1 = parseRule(rules)

    println("part 1")
    println(messages.count(isValid(_, generators1(0), generators1)))

    val rules2 = rules ++ Map(
      8 -> "8: 42 | 42 8 ",
      11 -> "42: 42 31 | 42 11 31 ",
    )

    val generators2 = parseRule(rules2)
    println("part 2")
    println(messages.count(isValid(_, generators2(0), generators2)))

}
