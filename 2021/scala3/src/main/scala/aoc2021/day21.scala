package aoc2021

object day21 {

  final case class Player(id: Int, position: Int, score: Long)

  def part1(p1: Player, p2: Player): Long  = {
    var rolls = 0
    var diceValue = 0
    var turn = 0
    var loser: Option[Player] = None
    var players = Array(p1, p2)
    while (players(0).score != 1000 && players(1).score != 1000) {
      var score = 0
      for(_ <- 0 until 3) {
        rolls  += 1
        diceValue = (diceValue % 100) + 1
        score += diceValue
      }
      var player = players(turn)
      var newPosition = (player.position + score) % 10
      players(turn) = player
                        .copy(position = newPosition,
                              score    = player.score + newPosition + 1)
      turn = (turn + 1) % 2
    }
    val loserScore = if (players(0).score < players(1).score) players(0).score else players(1).score
    return loserScore * rolls
  }


  def part2Big(p1: Player, p2: Player): Long = {
    type State = ((Player, Player), Int, Int, Int)
    val memo = scala.collection.mutable.Map.empty[State, (Long, Long)]
    val rollsAllowed = 3
    def countWins(ps: (Player, Player), turn: Int, rollsLeft: Int, rollsScore: Int): (Long,Long) = {
      val s = (ps, turn, rollsLeft, rollsScore)
      ps match {
        case (p1, _) if p1.score >= 21 => return (1, 0)
        case (_, p2) if p2.score >= 21 => return (0, 1)
        case _ if memo.contains(s) => return memo(s)
        case (p1, p2) if rollsLeft == 0 =>
          var player = if (turn == 0) p1 else p2
          val newPosition = (player.position + rollsScore) % 10
          var playerUpdated = player.copy(position = newPosition, score = player.score + newPosition + 1)
          val nps = if (turn == 0) (playerUpdated, p2) else (p1, playerUpdated)
          countWins(nps, 1-turn, rollsAllowed, 0)
        case _ =>
          val res1 = countWins(ps, turn, rollsLeft - 1, rollsScore + 1)
          val res2 = countWins(ps, turn, rollsLeft - 1, rollsScore + 2)
          val res3 = countWins(ps, turn, rollsLeft - 1, rollsScore + 3)
          val res = (res1._1 + res2._1 + res3._1, res1._2 + res2._2 + res3._2)
          memo(s) = res
          return res
      }
    }

    val wins = countWins((p1, p2), 0, rollsAllowed, 0)
    val winner = if wins._1 > wins._2 then wins._1 else wins._2
    winner
  }


  def main(args: Array[String]): Unit = {
    println(part1(Player(1, 7, 0), Player(2, 3, 0)))
    println(part2Big(Player(1, 7, 0), Player(2, 3, 0)))
  }
}

