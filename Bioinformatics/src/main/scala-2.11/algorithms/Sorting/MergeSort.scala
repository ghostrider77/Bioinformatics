package algorithms.Sorting

/**
  * problem description: http://rosalind.info/problems/ms/
  */

object MergeSort {

  object SampleData {
    val sample: List[String] =
      List(
        "10",
        "20 19 35 -18 17 -20 20 1 4 4"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}

  val inputFileName: String = "/algorithms/datasets/rosalind_ms.txt"

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data(1))
  }

  def mergeSortedArrays(first: List[Int], second: List[Int]): List[Int] = {

    @tailrec
    def merge(xs: List[Int], ys: List[Int], acc: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, Nil) => acc
      case (Nil, y :: yss) => merge(Nil, yss, y :: acc)
      case (x :: xss, Nil) => merge(xss, Nil, x :: acc)
      case (x :: xss, y :: yss) => if (x < y) merge(xss, ys, x :: acc) else merge(xs, yss, y :: acc)
    }

    merge(first, second, Nil).reverse
  }

  def mergeSort(array: List[Int]): List[Int] = {
    val length: Int = array.length
    if (length <= 1) array
    else {
      val (first, second): (List[Int], List[Int]) = array.splitAt(length / 2)
      val sortedFirst: List[Int] = mergeSort(first)
      val sortedSecond: List[Int] = mergeSort(second)
      mergeSortedArrays(sortedFirst, sortedSecond)
    }
  }

  def main(args: Array[String]): Unit = {
    val array: List[Int] = getData(isPractice = false)
    val result: List[Int] = mergeSort(array)
    writeListAsStringToFile(result)
  }

}
