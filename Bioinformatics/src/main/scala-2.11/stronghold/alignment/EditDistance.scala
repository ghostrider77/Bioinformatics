package stronghold.alignment

/**
  * problem description: http://rosalind.info/problems/edit/
  */

object EditDistance {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_39",
        "PLEASANTLY",
        ">Rosalind_11",
        "MEANLY"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, readFastaSequences}

  val inputFileName: String = "/stronghold/datasets/rosalind_edit.txt"

  def getData(isPractice: Boolean): List[String] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readFastaSequences(data, isLineSeparated = false).map(_.string)
  }

  def calcLevenshteinDistance(string1: String, string2: String): Int = {
    val n: Int = string1.length
    val m: Int = string2.length
    val editDistance: Array[Array[Int]] = Array.fill(n + 1, m + 1)(0)
    def matchingScore(c1: Char, c2: Char): Int = if (c1 == c2) 0 else 1

    for { ix <- 1 to n } editDistance(ix)(0) = ix
    for { jy <- 1 to m } editDistance(0)(jy) = jy

    for {
      ix <- 1 to n
      jy <- 1 to m
    } {
      val deletion: Int = editDistance(ix - 1)(jy) + 1
      val insertion: Int = editDistance(ix)(jy - 1) + 1
      val matching: Int = editDistance(ix - 1)(jy - 1) + matchingScore(string1(ix - 1), string2(jy - 1))
      editDistance(ix)(jy) = List(insertion, deletion, matching).min
    }

    editDistance(n)(m)
  }

  def main(args: Array[String]): Unit = {
    val List(string1, string2): List[String] = getData(isPractice = false)
    val result: Int = calcLevenshteinDistance(string1, string2)
    println(result)
  }

}
