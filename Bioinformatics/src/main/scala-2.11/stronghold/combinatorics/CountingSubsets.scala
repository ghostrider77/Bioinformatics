package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/sset/
  */

object CountingSubsets {

  object SampleData {
    val sample: List[String] = List("3")
  }

  import SampleData.sample
  import utils.UtilityFunctions.readInputData

  val inputFileName: String = "/stronghold/datasets/rosalind_sset.txt"

  private val Modulus: Int = 1000000

  def getData(isPractice: Boolean): Int = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.head.toInt
  }

  def calcNumberOfSubsets(n: Int): Int = (0 until n).foldLeft(1)((acc, _) => (acc * 2) % Modulus)

  def main(args: Array[String]): Unit = {
    val n: Int = getData(isPractice = false)
    val result: Int = calcNumberOfSubsets(n)
    println(result)
  }

}
