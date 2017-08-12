package stronghold.alignment

/**
  * problem description: http://rosalind.info/problems/edta/
  */

object EditDistanceAlignment {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_43",
        "PRETTY",
        ">Rosalind_97",
        "PRTTEIN"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{Alignment, readInputData, readFastaSequences, writeAlignmentToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_edta.txt"

  private val GapSymbol: Char = '-'

  def getData(isPractice: Boolean): List[String] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readFastaSequences(data, isLineSeparated = false).map(_.string)
  }

  def calcGlobalAlignmentUsingHammingDistance(string1: String, string2: String): Alignment = {
    val n: Int = string1.length
    val m: Int = string2.length
    val longestPath: Array[Array[Int]] = Array.fill[Int](n + 1, m + 1)(0)
    val backtrack: Array[Array[Byte]] = Array.fill[Byte](n, m)(0)
    def matchingScore(c1: Char, c2: Char): Int = if (c1 == c2) 0 else 1

    for { ix <- 1 to n } longestPath(ix)(0) = ix
    for { jy <- 1 to m } longestPath(0)(jy) = jy

    for {
      ix <- 1 to n
      jy <- 1 to m
    } {
      val deletion: Int = longestPath(ix - 1)(jy) + 1
      val insertion: Int = longestPath(ix)(jy - 1) + 1
      val matching: Int = longestPath(ix - 1)(jy - 1) + matchingScore(string1(ix - 1), string2(jy - 1))
      val nodeWithLargestValue: Int = List(deletion, insertion, matching).min
      longestPath(ix)(jy) = nodeWithLargestValue

      if (nodeWithLargestValue == deletion) backtrack(ix - 1)(jy - 1) = -1
      else if (nodeWithLargestValue == insertion) backtrack(ix - 1)(jy - 1) = 1
      else backtrack(ix - 1)(jy - 1) = 0
    }

    val alignmentScore: Int = longestPath(n)(m)
    val (alignedString1, alignedString2): (String, String) = getGlobalAlignment(backtrack, string1, string2, n, m)
    Alignment(alignedString1, alignedString2, alignmentScore)
  }

  def getGlobalAlignment(backtrack: Array[Array[Byte]],
                         string1: String,
                         string2: String,
                         n: Int,
                         m: Int): (String, String) = {
    @tailrec
    def assemble(ix: Int, jy: Int, alignedString1: List[Char], alignedString2: List[Char]): (String, String) = {
      if (ix > 0 && jy > 0) {
        val previousNodeOnGrid: Int = backtrack(ix - 1)(jy - 1)
        if (previousNodeOnGrid == 0)
          assemble(ix - 1, jy - 1, string1(ix - 1) :: alignedString1, string2(jy  -1) :: alignedString2)
        else if (previousNodeOnGrid == -1)
          assemble(ix - 1, jy, string1(ix - 1) :: alignedString1, GapSymbol :: alignedString2)
        else assemble(ix, jy - 1, GapSymbol :: alignedString1, string2(jy - 1) :: alignedString2)
      }
      else if (ix > 0 && jy == 0)
        assemble(ix - 1, 0, string1(ix - 1) :: alignedString1, GapSymbol :: alignedString2)
      else if (ix == 0 && jy > 0)
        assemble(0, jy - 1, GapSymbol :: alignedString1, string2(jy - 1) :: alignedString2)
      else (alignedString1.mkString(""), alignedString2.mkString(""))
    }

    assemble(n, m, Nil, Nil)
  }

  def main(args: Array[String]): Unit = {
    val List(string1, string2): List[String] = getData(isPractice = false)
    val result: Alignment = calcGlobalAlignmentUsingHammingDistance(string1, string2)
    writeAlignmentToFile(result)
  }

}
