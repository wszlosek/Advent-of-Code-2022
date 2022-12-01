import scala.collection.mutable.ListBuffer

object ListOperations {
  def splitAt(listToSplit: List[String], element: String): List[List[String]] = {
    val result = ListBuffer[List[String]]()
    val elementIndexes = indexesOfElement(listToSplit, element)
    var n = 0

    elementIndexes.foreach(index => {
      result += listToSplit.slice(n, index)
      n = index + 1
    })
    result += listToSplit.slice(n, listToSplit.size)

    result.toList
  }

  def indexesOfElement(list: List[String], element: String) : List[Int] = {
    list
      .zipWithIndex
      .filter(pair => pair._1 == element)
      .map(pair => pair._2)
  }
}
