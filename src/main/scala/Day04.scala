import utils.{FileReader, ListOperations}

import scala.collection.mutable.ListBuffer

object Day04 {

  val file: FileReader = FileReader("day04-input.txt")
  val fileContent: List[String] = file.fileContent()

  def main(args: Array[String]): Unit = println(s"1: $part1; 2: $part2")

  def part1: Int = countRelationsBetweenGroups(relationContains)

  def part2: Int = countRelationsBetweenGroups(relationExists)

  def splitIntoGroups(fileContent: List[String], firstRegex: String, secondString: String): ListBuffer[List[String]] = {
    val result = ListBuffer[List[String]]()
    fileContent.foreach(line => {
      result += line.split(firstRegex)(0).split(secondString).toList
      result += line.split(firstRegex)(1).split(secondString).toList
    })

    result
  }

  def relationContains(u0: List[Int], u1: List[Int]): Boolean = u0.forall(u1.contains)

  def relationExists(u0: List[Int], u1: List[Int]): Boolean = u0.exists(u1.contains)

  def countRelationsBetweenGroups(relation: (List[Int], List[Int]) => Boolean): Int = {

    splitIntoGroups(fileContent, firstRegex = ",", secondString = "-")
      .map(el => Range(el(0).toInt, el(1).toInt + 1))
      .grouped(2)
      .flatMap(u => {
        if (relation(u(0).toList, u(1).toList) || relation(u(1).toList, u(0).toList)) "."
        else None
      })
      .size
  }
}
