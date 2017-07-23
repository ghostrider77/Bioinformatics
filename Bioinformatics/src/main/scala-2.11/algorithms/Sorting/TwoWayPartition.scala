package algorithms.Sorting

/**
  * problem description: http://rosalind.info/problems/par/
  */

object TwoWayPartition {

  object SampleData {
    val sample: List[String] =
      List(
        "9",
        "7 2 5 6 1 3 9 4 8"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}

  val inputFileName: String = "/algorithms/datasets/rosalind_par.txt"

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data(1))
  }

  def partition(array: List[Int], pivot: Int): List[Int] = {
    val (first, second): (List[Int], List[Int]) = array.partition(_ <= pivot)
    first ::: (pivot :: second)
  }

  def main(args: Array[String]): Unit = {
    val array: List[Int] = getData(isPractice = false)
    val result: List[Int] = partition(array.tail, array.head)
    writeListAsStringToFile(result)
  }

}
