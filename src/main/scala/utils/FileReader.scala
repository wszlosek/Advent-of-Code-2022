package utils

import scala.io.Source

case class FileReader(filename: String) {
  
  def readInput(): String = {
    val fileSource = Source.fromFile(s"inputs/$filename")
    try fileSource.mkString finally fileSource.close()
  }

  def fileContent(): List[String] = {
    readInput().linesIterator.toList
  }
}
