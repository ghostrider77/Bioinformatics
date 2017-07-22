package algorithms.Sorting

/**
  * problem description: http://rosalind.info/problems/ins/
  */

object InsertionSort {

  object SampleData {
    val sample: List[String] =
      List(
        "6",
        "6 10 4 5 1 2"
      )
  }

  val inputFileName: String = "/algorithms/datasets/rosalind_ins.txt"

  import scala.annotation.tailrec
  import SampleData.sample
  import algorithms.Utils.UtilityFunctions.{readInputData, convertStringToIntList}

  def getData(isPractice: Boolean): (Int, Array[Int]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val n: Int = data.head.toInt
    val array: Array[Int] = convertStringToIntList(data(1)).toArray
    (n, array)
  }

  def calcSwaps(array: Array[Int], ix: Int): Int = {

    @tailrec
    def loop(swaps: Int, k: Int): Int = {
      if (k == 0 || array(k) >= array(k - 1)) swaps
      else {
        val elem: Int = array(k)
        array(k) = array(k - 1)
        array(k - 1) = elem
        loop(swaps + 1, k - 1)
      }
    }

    loop(0, ix)
  }

  def calcNumberOfSwaps(array: Array[Int], length: Int): Int =
    (1 until length).foldLeft(0)((acc, ix) => acc + calcSwaps(array, ix))

  def main(args: Array[String]): Unit = {
    val (n, array): (Int, Array[Int]) = getData(isPractice = false)
    val result: Int = calcNumberOfSwaps(array, n)
    println(result)
  }

}
