package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/lcsq/
  */

object FindingASharedSplicedMotif {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_23",
        "AACCTTGG",
        ">Rosalind_64",
        "ACACTGTGA"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, readFastaSequences, writeListAsStringToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_lcsq.txt"

  def getData(isPractice: Boolean): List[String] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readFastaSequences(data, isLineSeparated = false).map(_.string)
  }

  def calcLongestCommonSubsequence(string1: String, string2: String): String = {
    val n: Int = string1.length
    val m: Int = string2.length

    val longestPath: Array[Array[Int]] = Array.fill(n + 1, m + 1)(0)
    val backtrack: Array[Array[Byte]] = Array.fill(n, m)(0)
    def matchReward(c1: Char, c2: Char): Int = if (c1 == c2) 1 else 0

    for {
      ix <- 1 to n
      jy <- 1 to m
    } {
      val insertion: Int = longestPath(ix - 1)(jy)
      val deletion: Int = longestPath(ix)(jy - 1)
      val matchingCharacters: Int = longestPath(ix - 1)(jy - 1) + matchReward(string1(ix - 1), string2(jy - 1))
      val largestValue: Int = List(insertion, deletion, matchingCharacters).max

      longestPath(ix)(jy) = largestValue
      if (largestValue == insertion) backtrack(ix - 1)(jy - 1) = -1
      else if (largestValue == deletion) backtrack(ix - 1)(jy - 1) = 1
      else backtrack(ix - 1)(jy - 1) = 0
    }

    assembleLongestCommonSubsequence(backtrack, string1, n, m)
  }

  def assembleLongestCommonSubsequence(backtrack: Array[Array[Byte]], string: String, n: Int, m: Int): String = {
    @tailrec
    def assemble(ix: Int, jy: Int, subsequence: List[Char]): List[Char] = {
      if (ix <= 0 || jy <= 0) subsequence
      else {
        val previousNodeOnGrid: Int = backtrack(ix - 1)(jy - 1)
        if (previousNodeOnGrid == 0) assemble(ix - 1, jy - 1, string(ix - 1) :: subsequence)
        else if (previousNodeOnGrid == -1) assemble(ix - 1, jy, subsequence)
        else assemble(ix, jy - 1, subsequence)
      }
    }

    assemble(n, m, Nil).mkString("")
  }

  def main(args: Array[String]): Unit = {
    val List(string1, string2): List[String] = getData(isPractice = false)
    val result: String = calcLongestCommonSubsequence(string1, string2)
    writeListAsStringToFile(result.toList, sep = "")
  }

}
