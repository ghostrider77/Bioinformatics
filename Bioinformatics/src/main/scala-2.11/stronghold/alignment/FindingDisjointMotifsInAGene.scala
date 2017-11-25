package stronghold.alignment

/**
  * problem description: http://rosalind.info/problems/itwv/
  */

object FindingDisjointMotifsInAGene {

  object SampleData {
    val sample: List[String] =
      List(
        "GACCACGGTT",
        "ACAG",
        "GT",
        "CCG"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_itwv.txt"

  def getData(isPractice: Boolean): (String, List[String]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    (data.head, data.tail)
  }

  def patternsCanBeInterwovenInSubstring(substring: String, pattern1: String, pattern2: String): Boolean = {
    val n: Int = pattern1.length
    val m: Int = pattern2.length
    val substringCoverage: Array[Array[Boolean]] = Array.fill[Boolean](n + 1, m + 1)(false)

    substringCoverage(0)(0) = true
    for { ix <- 0 until n } substringCoverage(ix + 1)(0) = substringCoverage(ix)(0) && pattern1(ix) == substring(ix)
    for { jy <- 0 until m } substringCoverage(0)(jy + 1) = substringCoverage(0)(jy) && pattern2(jy) == substring(jy)
    for {
      ix <- 0 until n
      jy <- 0 until m
    } {
      val char: Char = substring(ix + jy + 1)
      val vertical: Boolean = substringCoverage(ix)(jy + 1) && pattern1(ix) == char
      val horizontal: Boolean = substringCoverage(ix + 1)(jy) && pattern2(jy) == char
      substringCoverage(ix + 1)(jy + 1) = vertical || horizontal
    }

    substringCoverage(n)(m)
  }

  def canPatternsBeInterwovenInDna(dna: String, pattern1: String, pattern2: String): Boolean = {
    val substringLength: Int = pattern1.length + pattern2.length
    if (dna.length < substringLength) false
    else dna.sliding(substringLength).exists(patternsCanBeInterwovenInSubstring(_, pattern1: String, pattern2: String))
  }

  def calcFeasibilityMatrix(dna: String, patterns: List[String]): Array[Array[Int]] = {
    val numberOfPatterns: Int = patterns.length
    val matrix: Array[Array[Int]] = Array.fill[Int](numberOfPatterns, numberOfPatterns)(0)
    for {
      (pattern1, ix) <- patterns.zipWithIndex
      (pattern2, jy) <- patterns.take(ix + 1).zipWithIndex
    } {
      val verdict: Int = if (canPatternsBeInterwovenInDna(dna, pattern1, pattern2)) 1 else 0
      matrix(ix)(jy) = verdict
      matrix(jy)(ix) = verdict
    }

    matrix
  }

  def main(args: Array[String]): Unit = {
    val (dna, patterns): (String, List[String]) = getData(isPractice = false)
    val feasibilityMatrix: Array[Array[Int]] = calcFeasibilityMatrix(dna, patterns)
    writeListOfListsAsStringsToFile(feasibilityMatrix.map(_.toList).toList)
  }

}
