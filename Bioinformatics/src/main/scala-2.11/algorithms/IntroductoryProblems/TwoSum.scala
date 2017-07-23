package algorithms.IntroductoryProblems

/**
  * problem description: http://rosalind.info/problems/2sum/
  */

object TwoSum {

  object SampleData {
    val sample: List[String] =
      List(
        "4 5",
        "2 -3 4 10 5",
        "8 2 4 -2 -8",
        "-5 2 3 2 -4",
        "5 4 -5 6 8"
      )
  }

  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/algorithms/datasets/rosalind_2sum.txt"

  def getData(isPractice: Boolean): List[List[Int]] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    for { line <- data.tail } yield convertStringToIntList(line)
  }

  def find2SumIndicesForArray(array: List[Int], x: Int = 0): List[Int] = {
    val map: MutableMap[Int, Int] = MutableMap.empty

    @tailrec
    def loop(as: List[Int], i: Int): List[Int] = {
      if (as.isEmpty) List(-1)
      else {
        val elem: Int = as.head
        map.get(elem) match {
          case Some(j) => List(j, i)
          case None =>
            map += (x - elem) -> i
            loop(as.tail, i + 1)
        }
      }
    }

    loop(array, 0)
  }

  def find2SumIndices(arrays: List[List[Int]]): List[List[Int]] =
    for { array <- arrays } yield find2SumIndicesForArray(array)

  def correctToOneBasedIndexing(indices: List[Int]): List[Int] = indices.map(ix => if (ix == -1) ix else ix + 1)

  def main(args: Array[String]): Unit = {
    val arrays: List[List[Int]] = getData(isPractice = false)
    val result: List[List[Int]] = find2SumIndices(arrays).map(correctToOneBasedIndexing)
    writeListOfListsAsStringsToFile(result)
  }

}
