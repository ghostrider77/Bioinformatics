package algorithms.Sorting

/**
  * problem description: http://rosalind.info/problems/mer/
  */

object MergeTwoSortedArrays {

  object SampleData {
    val sample: List[String] =
      List(
        "4",
        "2 4 10 18",
        "3",
        "-5 11 12"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import algorithms.Utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}

  val inputFileName: String = "/algorithms/datasets/rosalind_mer.txt"

  def getData(isPractice: Boolean): (List[Int], List[Int]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val first: List[Int] = convertStringToIntList(data(1))
    val second: List[Int] = convertStringToIntList(data(3))
    (first, second)
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

  def main(args: Array[String]): Unit = {
    val (first, second): (List[Int], List[Int]) = getData(isPractice = false)
    val result: List[Int] = mergeSortedArrays(first, second)
    writeListAsStringToFile(result)
  }

}
