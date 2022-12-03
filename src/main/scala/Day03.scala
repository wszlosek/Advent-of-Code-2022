import utils.FileReader

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

object Day03 {

  def main(args: Array[String]): Unit = {
    val file = FileReader("day03-input.txt")
    val fileContent = file.fileContent()

    println(s"1: ${part1(fileContent)}; 2: ${part2(fileContent)}")
  }

  def part1(fileContent: List[String]): Int = {
    val part1Groups = fileContent
      .map(w => w.splitAt(w.length / 2))
      .map(w => commonLetters(w._1, w._2))

    calculateResult(part1Groups)
  }

  def part2(fileContent: List[String]): Int = {
    val part2Groups = fileContent
      .grouped(3)
      .toList
      .map(w => commonLetters(w))

    calculateResult(part2Groups)
  }

  def commonLetters(s1: String, s2: String): String = {
    (s1 intersect s2).distinct
  }

  def commonLetters(l: List[String]): String = {
    val lb = ListBuffer[String]()
    l.combinations(2).toList.foreach(w => {
        lb += commonLetters(w.head, w(1))
    })

    lb.zip(lb.drop(1)).map {
      case (f, s) => f intersect s
    } mkString
  }

  def calculateResult(groupsOfCommonLetters: List[String]): Int = {
    groupsOfCommonLetters
      .map(w =>
        if (w(0).isLower) w(0).toInt - 'a' + 1
        else w(0).toInt - 'A' + 27
      ).sum
  }
}
