package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/pper/
  */

object PartialPermutations {

  object SampleData {
    val sample: List[String] = List("21 7")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}

  val inputFileName: String = "/stronghold/datasets/rosalind_pper.txt"

  private val Modulus: Int = 1000000

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data.head)
  }

  def calcNumberOfPartialPermutations(n: Int, k: Int): Long =
    (n until n - k by -1).foldLeft(1L)((acc, m) => (acc * m) % Modulus )

  def main(args: Array[String]): Unit = {
    val List(n, k): List[Int] = getData(isPractice = false)
    val result: Long = calcNumberOfPartialPermutations(n, k)
    println(result)
  }

}
