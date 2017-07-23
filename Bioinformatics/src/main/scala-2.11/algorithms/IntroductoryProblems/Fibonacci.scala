package algorithms.IntroductoryProblems

/**
  * problem description: http://rosalind.info/problems/fibo/
  */

object Fibonacci {

  object SampleData {
    val sample: List[String] = List("6")
  }

  import SampleData.sample
  import utils.UtilityFunctions.readInputData

  val inputFileName: String = "/algorithms/datasets/rosalind_fibo.txt"

  def getData(isPractice: Boolean): Int = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.head.toInt
  }

  def calcFibonacciNumber(n: Int): Long = (0 until n).foldLeft((0L, 1L)){ case ((a, b), _) => (b, a + b) }._1

  def main(args: Array[String]): Unit = {
    val n: Int = getData(isPractice = false)
    val result: Long = calcFibonacciNumber(n)
    println(result)
  }

}
