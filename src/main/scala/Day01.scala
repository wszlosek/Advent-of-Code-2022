import scala.collection.mutable.ListBuffer

object Day01 {
  def main(args: Array[String]): Unit = {
    val file = FileReader("day01-input.txt")
    val fileContent = file.fileContent()
    println(s"1: ${part1(fileContent)}; 2: ${part2(fileContent)}")
  }

  def part1(fileContent: List[String]): Int = {
    partialSums(fileContent).max
  }

  def part2(fileContent: List[String]): Int = {
    partialSums(fileContent)
      .sorted
      .takeRight(3)
      .sum
  }

  def partialSums(fileContent: List[String]): List[Int] = {
    import ListOperations._
    splitAt(fileContent,"")
      .map(_.map(_.toInt).sum)
  }
}
