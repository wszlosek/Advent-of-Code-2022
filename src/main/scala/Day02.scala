import utils.FileReader

import scala.collection.mutable.ListBuffer
import scala.collection.*

object Day02 {

  def main(args: Array[String]): Unit = {
    val file = FileReader("day02-input.txt")
    val fileContent = file.fileContent()

    println(s"1: ${part1(fileContent)}; 2: ${part2(fileContent)}")
  }

  def part1(fileContent: List[String]): Int = {
    rounds(fileContent)
      .map(_.pointResult())
      .sum
  }

  def part2(fileContent: List[String]): Int = {
    rounds(fileContent, false)
      .map(_.pointResult())
      .sum
  }

  case class Round(p1: GameMark, p2: GameMark) {

    def verdict(): GameMark = {
      val sum = p1.getValue + p2.getValue
      sum match {

        case _ if p1 == p2 => GameMark.Draw

        case rockWin if rockWin == GameMark.Rock.getValue + GameMark.Scissors.getValue =>
          if (p1 == GameMark.Rock) p1 else p2

        case paperWin if paperWin == GameMark.Paper.getValue + GameMark.Rock.getValue =>
          if (p1 == GameMark.Paper) p1 else p2

        case scissorsWin if scissorsWin == GameMark.Scissors.getValue + GameMark.Paper.getValue =>
          if (p1 == GameMark.Scissors) p1 else p2
      }
    }

    def pointResult(): Int = {
      val winner = verdict()
      winner match {
        case this.p1 => GameMark.Lost.getValue + p2.getValue
        case this.p2 => GameMark.Win.getValue + p2.getValue
        case _ => GameMark.Draw.getValue + p2.getValue
      }
    }
  }

  def predict(p1: GameMark, p2: GameMark): GameMark = p2 match {

    case GameMark.Draw => GameMark
      .fromOrdinal(p1.ordinal)

    case GameMark.Lost => opposites
      .filter(_._2 == p1)
      .keys
      .toList
      .head

    case GameMark.Win => opposites(p1)
  }

  var opposites: Map[GameMark, GameMark] = Map(
    GameMark.Rock -> GameMark.Paper,
    GameMark.Paper -> GameMark.Scissors,
    GameMark.Scissors -> GameMark.Rock
  )

  enum GameMark(mark: String, value: Int) {
    def getMark: String = mark
    def getValue: Int = value

    case Rock extends GameMark("A", 1)
    case Paper extends GameMark("B", 2)
    case Scissors extends GameMark("C", 3)
    case Lost extends GameMark("X", 0)
    case Draw extends GameMark("Y", 3)
    case Win extends GameMark("Z", 6)
  }

  def getMark(p: String): GameMark = p match {
    case rock if GameMark.Rock.getMark.contains(p) => GameMark.Rock
    case paper if GameMark.Paper.getMark.contains(p) => GameMark.Paper
    case scissors if GameMark.Scissors.getMark.contains(p) => GameMark.Scissors
    case lost if GameMark.Lost.getMark.contains(p) => GameMark.Lost
    case draw if GameMark.Draw.getMark.contains(p) => GameMark.Draw
    case win if GameMark.Win.getMark.contains(p) => GameMark.Win
  }

  def rounds(fileContent: List[String], fairRules: Boolean = true): List[Round] = {
    fileContent.map(_.split(" ") match {
      case Array(p1, p2) =>
        val _p2 = if (fairRules) {
          p2 match {
            case "X" => getMark("A")
            case "Y" => getMark("B")
            case "Z" => getMark("C")
          }
        } else {
          predict(getMark(p1), getMark(p2))
        }

        Round(getMark(p1), _p2)
    })
  }
}