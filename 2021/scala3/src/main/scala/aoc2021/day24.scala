package aoc2021

import aoc2021.day24.execute

import scala.util.Random
import scala.collection.mutable.{Map => MMap}

object day24 {

  import Token._
  import Command._

  enum Token:
    case Literal(val v: Long)
    case Variable(val v: Char)

  enum Command:
    case Inp(a: Variable)
    case Add(a: Variable, b: Token)
    case Mul(a: Variable, b: Token)
    case Mod(a: Variable, b: Token)
    case Div(a: Variable, b: Token)
    case Eql(a: Variable, b: Token)

  final case class Environment(registers: Map[Char, Long])

  object Environment:
    def apply(): Environment = new Environment(Map('x' -> 0, 'y' -> 0, 'z' -> 0, 'w' -> 0))

  def parse(s: String): Command =
    def parseArg(arg: String): Token = try Token.Literal(arg.toInt) catch case _ => Token.Variable(arg.head)
    val (head, args) = (s.split(" ").head, s.split(" ").tail)
    head match
      case "inp" => Inp(Variable(args(0).head))
      case "mul" =>
        Mul(Variable(args(0).head), parseArg(args(1)))
      case "add" =>
        Add(Variable(args(0).head), parseArg(args(1)))
      case "mod" =>
        Mod(Variable(args(0).head), parseArg(args(1)))
      case "div" =>
        Div(Variable(args(0).head), parseArg(args(1)))
      case "eql" =>
        Eql(Variable(args(0).head), parseArg(args(1)))


  def parse(s: Seq[String]): Seq[Command] = s.map(parse)




  def execute(command: Command, result: Environment, inputStream: Iterator[Int]): Environment =
    command match
      case Inp(a) =>
        val in = inputStream.next()
        result.copy(registers = result.registers + (a.v -> in))
      case Add(a, Literal(b)) =>
        result.copy(registers = result.registers + (a.v -> (result.registers(a.v) + b)))
      case Add(a, Variable(b)) =>
        result.copy(registers = result.registers + (a.v -> (result.registers(a.v) + result.registers(b))))
      case Mul(a, Literal(b)) =>
        result.copy(registers = result.registers + (a.v -> (result.registers(a.v) * b)))
      case Mul(a, Variable(b)) =>
        result.copy(registers = result.registers + (a.v -> (result.registers(a.v) * result.registers(b))))
      case Div(a, Literal(b)) =>
        result.copy(registers = result.registers + (a.v -> (result.registers(a.v) / b)))
      case Div(a, Variable(b)) =>
        result.copy(registers = result.registers + (a.v -> (result.registers(a.v) / result.registers(b))))
      case Mod(a, Literal(b)) =>
        result.copy(registers = result.registers + (a.v -> (result.registers(a.v) % b)))
      case Mod(a, Variable(b)) =>
        result.copy(registers = result.registers + (a.v -> (result.registers(a.v) % result.registers(b))))
      case Eql(a, Literal(b)) =>
        result.copy(registers = result.registers + (a.v -> (if (result.registers(a.v) == b) 1 else 0)))
      case Eql(a, Variable(b)) =>
        result.copy(registers = result.registers + (a.v -> (if (result.registers(a.v) == result.registers(b)) 1 else 0)))


  def execute(program: Seq[Command], environment: Environment, inputStream: Iterator[Int]): Environment =
    program.foldLeft(Environment())((result, command) => execute(command, result, inputStream))

  def executeProgram(program: Seq[Command], environment: Environment, inputStream: Iterator[Int]): Environment =
      program.foldLeft(environment)((result, command) => execute(command, result, inputStream))

  def execute(program: String, iterator: Iterator[Int]): Environment = execute(parse(program.split("\n")) , Environment(), iterator)

  def test1() = {
    val prog1 =
      """
        |inp x
        |mul x -1
        |""".stripMargin.trim
    val res = execute(prog1, Iterator(1))
    assert(res.registers('x') == -1)
  }

  def test2() = {
    val prog1 =
      """
        |inp z
        |inp x
        |mul z 3
        |eql z x
        |""".stripMargin.trim
    val res1 = execute(prog1, Iterator(1, 2))

    assert(res1.registers('z') == 0)
    val res2 = execute(prog1, Iterator(1, 3))
    assert(res2.registers('z') == 1 && res2.registers('x') == 3)
  }

  def runTests() = {
    test1()
    test2()
  }

  def readMonadProgram(inputFile: String): String = scala.io.Source.fromFile(inputFile).mkString

  def validate(prog: Seq[Command], dir: Int) = {

    val (from, to) = if dir < 0 then (9, 1) else (1, 9)


    val memo = MMap.empty[(Int, Environment), Boolean]
    def execute(num: Long, pos: Int, progPos: Int, env: Environment): Boolean = {
      if (pos > 14) return false
      if (memo.contains(pos, env)) return memo((pos, env))
      if (pos == 14) {
        memo((pos, env)) = false
        val res = executeProgram(prog.slice(progPos, prog.size).toSeq, env, Iterator.empty[Int])
        if (res.registers('z') == 0) {
          println(s"for $num we get $res")
          memo((pos, env)) = true
        }
        return memo((pos, env))
      }

      if (env.registers('z') > 26 * 26 * 26 * 26 * 26) return false
      val nextInput = prog.indexWhere(x => x.isInstanceOf[Inp], progPos)
      var result = false
      for (x <- from to to by dir) {
        val iterator = Iterator(x)
        var res = executeProgram(prog.slice(progPos, nextInput+1), env, iterator)
        result ||= execute(num = num * 10 + x, pos+1, nextInput+1, res)
      }
      memo((pos, env)) = result
      return result
    }

    execute(num = 0, pos = 0, progPos = 0, env = Environment())
  }


  def part1() = {
    val input = readMonadProgram("./data/day24-input.txt")
    val prog = parse(input.split("\n"))
    validate(prog, -1)
  }

  def part2() = {
    val input = readMonadProgram("./data/day24-input.txt")
    val prog = parse(input.split("\n"))
    validate(prog, 1)
  }


  def main(args: Array[String]): Unit = {
    println("part1:")
    part1()
    println("part2:")
    part2()

  }
}
