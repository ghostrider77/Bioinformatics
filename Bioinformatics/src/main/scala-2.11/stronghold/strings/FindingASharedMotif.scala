package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/lcsm/
  */

object FindingASharedMotif {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_1",
        "GATTACA",
        ">Rosalind_2",
        "TAGACCA",
        ">Rosalind_3",
        "ATACA"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, readFastaSequences, writeListAsStringToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_lcsm.txt"

  def getData(isPractice: Boolean): List[String] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readFastaSequences(data, isLineSeparated = false).map(_.string)
  }

  def findCommonSubstringOfGivenLength(strings: List[String], shortestString: String,length: Int): Option[String] = {
    val allSubstrings: Iterator[String] = shortestString.sliding(length, 1)

    @tailrec
    def loop(): Option[String] = {
      if (!allSubstrings.hasNext) None
      else {
        val substring: String = allSubstrings.next()
        if (strings.forall(s => s.contains(substring))) Some(substring)
        else loop()
      }
    }

    loop()
  }

  def calcLongestCommonSubstring(strings: List[String]): Option[String] = {
    val shortestString: String = strings.minBy(_.length)
    val shortestLength: Int = shortestString.length

    @tailrec
    def loop(k: Int): Option[String] = {
      if (k == 0) None
      else {
        val commonSubstring: Option[String] = findCommonSubstringOfGivenLength(strings, shortestString, k)
        if (commonSubstring.isEmpty) loop(k - 1)
        else commonSubstring
      }
    }

    loop(shortestLength)
  }

  def main(args: Array[String]): Unit = {
    val strings: List[String] = getData(isPractice = false)
    val result: String = calcLongestCommonSubstring(strings).getOrElse("")
    writeListAsStringToFile(result.toList, sep = "")
  }

}
