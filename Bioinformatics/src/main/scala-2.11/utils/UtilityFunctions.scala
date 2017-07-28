package utils

import java.io.{BufferedWriter, File, FileWriter, InputStream}
import Nucleotide.toChar

object UtilityFunctions {

  def readInputData(filename: String): List[String] = {
    val fileStream: InputStream = getClass.getResourceAsStream(filename)
    val lines: Iterator[String] = scala.io.Source.fromInputStream(fileStream).getLines()
    lines.toList
  }

  def convertStringToIntList(line: String, sep: String = " "): List[Int] = line.split(sep).map(_.toInt).toList

  def writeListAsStringToFile[T](lst: List[T], outputFilename: String = "output.txt", sep: String = " "): Unit = {
    val file = new File(outputFilename)
    val bw = new BufferedWriter(new FileWriter(file))
    val result: String = lst.mkString(sep)
    bw.write(result)
    bw.close()
  }

  def writeListOfListsAsStringsToFile[T](lists: List[List[T]],
                                         outputFilename: String = "output.txt",
                                         sep: String = " "): Unit = {
    val file = new File(outputFilename)
    val bw = new BufferedWriter(new FileWriter(file))
    for { lst <- lists } {
      val line: String = lst.mkString(sep)
      bw.write(line)
      bw.write("\n")
    }
    bw.close()
  }

  def writeNucleotideListToFileAsString(nucleotides: List[Nucleotide],
                                        outputFilename: String = "output.txt"): Unit = {
    val file = new File(outputFilename)
    val bw = new BufferedWriter(new FileWriter(file))
    val result: String = nucleotides.map(toChar).mkString("")
    bw.write(result)
    bw.close()
  }

}
