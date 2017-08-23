package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/perm/
  */

object EnumeratingGeneOrders {

  object SampleData {
    val sample: List[String] = List("3")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_perm.txt"

  def getData(isPractice: Boolean): Int = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.head.toInt
  }

  def getPermutations(n: Int): List[List[Int]] = (1 to n).toList.permutations.toList

  def main(args: Array[String]): Unit = {
    val n: Int = getData(isPractice = false)
    val result: List[List[Int]] = getPermutations(n)
    writeListOfListsAsStringsToFile(List(result.length) :: result)
  }

}
