package algorithms.Sorting

/**
  * problem description: http://rosalind.info/problems/inv/
  */

object CountInversions {

  object SampleData {
    val sample: List[String] =
      List(
        "5",
        "-6 1 15 8 10"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.readInputData

  val inputFileName: String = "/algorithms/datasets/rosalind_inv.txt"

  def getData(isPractice: Boolean): List[Long] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data(1).split(" ").map(_.toLong).toList
  }

  def mergeSortedArrays(first: List[Long],
                        second: List[Long],
                        firstLength: Int,
                        inversions: Long): (List[Long], Long) = {

    @tailrec
    def merge(xs: List[Long],
              xsLength: Int,
              ys: List[Long],
              accList: List[Long],
              accInversions: Long): (List[Long], Long) = (xs, ys) match {
      case (Nil, Nil) => (accList, accInversions)
      case (Nil, y :: yss) => merge(Nil, xsLength, yss, y :: accList, accInversions)
      case (x :: xss, Nil) => merge(xss, xsLength - 1, Nil, x :: accList, accInversions)
      case (x :: xss, y :: yss) =>
        if (x <= y) merge(xss, xsLength - 1, ys, x :: accList, accInversions)
        else merge(xs, xsLength, yss, y :: accList, accInversions + xsLength)
    }

    val (sortedList, inversionsAfterMerge): (List[Long], Long) = merge(first, firstLength, second, Nil, inversions)
    (sortedList.reverse, inversionsAfterMerge)
  }

  def countInversions(array: List[Long]): (List[Long], Long) = {
    val length: Int = array.length
    if (length <= 1) (array, 0L)
    else {
      val middle: Int = length / 2
      val (first, second): (List[Long], List[Long]) = array.splitAt(middle)
      val (sortedFirst, inversionsInFirst): (List[Long], Long) = countInversions(first)
      val (sortedSecond, inversionsInSecond): (List[Long], Long) = countInversions(second)
      mergeSortedArrays(sortedFirst, sortedSecond, middle, inversionsInFirst + inversionsInSecond)
    }
  }

  def main(args: Array[String]): Unit = {
    val array: List[Long] = getData(isPractice = false)
    val (_, result): (List[Long], Long) = countInversions(array)
    println(result)
  }

}
