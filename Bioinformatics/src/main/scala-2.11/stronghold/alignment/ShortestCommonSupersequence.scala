package stronghold.alignment

/**
  * problem description: http://rosalind.info/problems/scsp/
  */

object ShortestCommonSupersequence {

  object SampleData {
    val sample: List[String] =
      List(
        "ATCTGAT",
        "TGCATA"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_scsp.txt"

  private val GapSymbol: Char = '-'

  def getData(isPractice: Boolean): List[String] = if (isPractice) sample else readInputData(inputFileName)

  def calcShortestCommonSupersequence(string1: String, string2: String): String = {
    val n: Int = string1.length
    val m: Int = string2.length
    val longestPath: Array[Array[Int]] = Array.fill[Int](n + 1, m + 1)(0)
    val backtrack: Array[Array[Byte]] = Array.fill[Byte](n, m)(0)

    for {
      ix <- 1 to n
      jy <- 1 to m
    } {
      val deletion: Int = longestPath(ix - 1)(jy)
      val insertion: Int = longestPath(ix)(jy - 1)
      val matching: Int = if (string1(ix - 1) == string2(jy - 1)) longestPath(ix - 1)(jy - 1) + 1 else Int.MinValue
      val nodeWithLargestValue: Int = List(deletion, insertion, matching).max
      longestPath(ix)(jy) = nodeWithLargestValue

      if (nodeWithLargestValue == deletion) backtrack(ix - 1)(jy - 1) = -1
      else if (nodeWithLargestValue == insertion) backtrack(ix - 1)(jy - 1) = 1
      else backtrack(ix - 1)(jy - 1) = 0
    }

    val (alignedString1, alignedString2): (String, String) = getGlobalAlignment(backtrack, string1, string2, n, m)
    (for { (letter1, letter2) <- alignedString1.zip(alignedString2) } yield {
      if (letter1 == GapSymbol) letter2 else letter1
    }).mkString("")
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
    val result: String = calcShortestCommonSupersequence(string1, string2)
    writeListAsStringToFile(result.toList, sep = "")
  }

}