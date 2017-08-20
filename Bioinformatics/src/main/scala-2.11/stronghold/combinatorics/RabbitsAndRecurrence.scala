package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/fib/
  */

object RabbitsAndRecurrence {

  object SampleData {
    val sample: List[String] = List("5 3")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}

  val inputFileName: String = "/stronghold/datasets/rosalind_fib.txt"

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data.head)
  }

  def calcGeneralizedFibonacciSequence(n: Int, k: Int): Long =
    (1 until n)
      .foldLeft((1L, 1L)){
        case ((numberOfReproductivePairs, totalNumberOfPairs), _) =>
          (totalNumberOfPairs, k * numberOfReproductivePairs + totalNumberOfPairs)
      }
      ._1

  def main(args: Array[String]): Unit = {
    val List(n, k): List[Int] = getData(isPractice = false)
    val result: Long = calcGeneralizedFibonacciSequence(n, k)
    println(result)
  }

}
