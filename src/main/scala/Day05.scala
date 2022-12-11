import utils.{FileReader, ListOperations}

import scala.collection.mutable.ListBuffer

object Day05 {

  val file: FileReader = FileReader("day05-input.txt")
  val fileContent: List[String] = file.fileContent()

  def main(args: Array[String]): Unit = {
    println(s"1:$part1; 2: $part2")
  }

  def part1: String = {
    swaps()
  }

  def part2: String = {
    swaps(reverse = false)
  }

  case class Instruction(len: Int, from: Int, to: Int)

  def startingStacks(): ListBuffer[ListBuffer[String]] = {

    ListBuffer(
      ListBuffer(), ListBuffer("F", "T", "C", "L", "R", "P", "G", "Q"),
      ListBuffer("N", "Q", "H", "W", "R", "F", "S", "J"), ListBuffer("F", "B", "H", "W", "P", "M", "Q"),
      ListBuffer("V", "S", "T", "D", "F"), ListBuffer("Q", "L", "D", "W", "V", "F", "Z"),
      ListBuffer("Z", "C", "L", "S"), ListBuffer("Z", "B", "M", "V", "D", "F"),
      ListBuffer("T", "J", "B"), ListBuffer("Q", "N", "B", "G", "L", "S", "P", "H")
    )
  }

  def instruction(): List[Instruction] = {

    val result = ListBuffer[Instruction]()

    fileContent
      .filter(_.contains("move"))
      .map(_.split(" "))
      .foreach(p => {
        result += Instruction(p(1).toInt, p(3).toInt, p(5).toInt)
      })

    result.toList
  }

  def swaps(reverse: Boolean = true): String = {
    val stacks = startingStacks()
    instruction().foreach(instruction => {
      val frag = stacks(instruction.from).takeRight(instruction.len)

      stacks(instruction.from) = stacks(instruction.from).take(stacks(instruction.from).length - frag.length)
      if (reverse) stacks(instruction.to) ++= frag.reverse else stacks(instruction.to) ++= frag
    })

    stacks.filter(_.nonEmpty)
      .map(u => u.last.head)
      .mkString
  }
}
