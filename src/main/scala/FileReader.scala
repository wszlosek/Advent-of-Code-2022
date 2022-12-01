import scala.io.Source

class FileReader(val filename: String) {
  def readInput(): String = {
    val fileSource = Source.fromFile(s"inputs/$filename")
    try fileSource.mkString finally fileSource.close()
  }

  def fileContent(): List[String] = {
    readInput().linesIterator.toList
  }
}
