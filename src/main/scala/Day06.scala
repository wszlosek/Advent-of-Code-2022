import utils.{FileReader, ListOperations}

import scala.collection.mutable.ListBuffer

object Day06 {

  val file: FileReader = FileReader("day06-input.txt")
  val fileContent: String = file.readInput()

  def main(args: Array[String]): Unit = {
    println(s"1:$part1; 2: $part2")
  }

  def part1: Int = {
    result(slidingAt = 4)
  }

  def part2: Int = {
    result(slidingAt = 14)
  }

  def containsDuplicates(s: String): Boolean = {
    s.distinct.length != s.length
  }

  def result(slidingAt: Int): Int = {
    val slides = fileContent.sliding(slidingAt).toList
    slides.length - slides.dropWhile(containsDuplicates).length + slidingAt
  }
}
