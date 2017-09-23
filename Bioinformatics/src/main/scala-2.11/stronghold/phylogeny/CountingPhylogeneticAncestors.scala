package stronghold.phylogeny

/**
  * problem description: http://rosalind.info/problems/inod/
  */

object CountingPhylogeneticAncestors {

  object SampleData {
    val sample: List[String] = List("4")
  }

  import SampleData.sample
  import utils.UtilityFunctions.readInputData

  val inputFileName: String = "/stronghold/datasets/rosalind_inod.txt"

  def getData(isPractice: Boolean): Int = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.head.toInt
  }

  def numberOfInnerNodesInUnrootedBinaryTree(numberOfLeaves: Int): Int = numberOfLeaves - 2

  def main(args: Array[String]): Unit = {
    val numberOfLeaves: Int = getData(isPractice = false)
    val result: Int = numberOfInnerNodesInUnrootedBinaryTree(numberOfLeaves)
    println(result)
  }
}
