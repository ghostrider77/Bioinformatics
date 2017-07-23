package algorithms.IntroductoryProblems

/**
  * problem description: http://rosalind.info/problems/bins/
  */

object BinarySearch {

  object SampleData {
    val sample: List[String] =
      List(
        "5",
        "6",
        "10 20 30 40 50",
        "40 10 35 15 40 20"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, convertStringToIntList, writeListAsStringToFile}

  val inputFileName: String = "/algorithms/datasets/rosalind_bins.txt"

  def getData(isPractice: Boolean): (Int, Vector[Int], List[Int]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val n: Int = data.head.toInt
    val array: Vector[Int] = convertStringToIntList(data(2)).toVector
    val elems: List[Int] = convertStringToIntList(data(3))
    (n, array, elems)
  }

  def binarySearch(array: Vector[Int], length: Int, elem: Int): Int = {

    @tailrec
    def searchForElemInRange(startIx: Int, endIx: Int): Int = {
      val midIx: Int = (startIx + endIx) / 2
      val middleElem: Int = array(midIx)

      if (startIx == endIx && elem != middleElem) -1
      else if (middleElem == elem) midIx
      else {
        if (middleElem < elem) searchForElemInRange(midIx + 1, endIx)
        else searchForElemInRange(startIx, midIx)
      }
    }

    searchForElemInRange(0, length - 1)
  }

  def findIndicesOfElems(array: Vector[Int], length: Int, elems: List[Int]): List[Int] =
    for {elem <- elems} yield binarySearch(array, length, elem)

  def changeToOneBasedIndexing(ix: Int): Int = if (ix == -1) -1 else ix + 1

  def main(args: Array[String]): Unit = {
    val (n, array, elems): (Int, Vector[Int], List[Int]) = getData(isPractice = false)
    val result: List[Int] = findIndicesOfElems(array, n, elems).map(changeToOneBasedIndexing)
    writeListAsStringToFile(result)
  }

}
