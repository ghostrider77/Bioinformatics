package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/fibd/
  */

object MortalFibonacciRabbits {

  object SampleData {
    val sample: List[String] = List("6 3")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}

  val inputFileName: String = "/stronghold/datasets/rosalind_fibd.txt"

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data.head)
  }

  def calcMortalFibonacciSequence(n: Int, m: Int): BigInt = {
    val initialAges: List[BigInt] = 1 :: List.fill[BigInt](m - 1)(0)
    (2 to n).foldLeft(initialAges){ case (ages, _) => ages.tail.sum :: ages.init }.sum
  }

  def main(args: Array[String]): Unit = {
    val List(n, m): List[Int] = getData(isPractice = false)
    val result: BigInt = calcMortalFibonacciSequence(n, m)
    println(result)
  }

}
