package stronghold.phylogeny

/**
  * problem description: http://rosalind.info/problems/nwck/
  */

object DistancesInTrees {
  type NewickTree = String
  type NodePair = (String, String)

  object SampleData {
    val sample: List[String] =
      List(
        "(cat)dog;",
        "dog cat",
        "",
        "(dog,cat);",
        "dog cat"
      )
  }

  import scala.util.matching.Regex
  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_nwck.txt"

  def getData(isPractice: Boolean): List[(NewickTree, NodePair)] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.grouped(3)
      .map{ triplet =>
        val Array(node1, node2): Array[String] = triplet(1).split(" ")
        (triplet.head, (node1, node2)) }
      .toList
  }

  def calcDistanceBetweenNodes(newickTree: NewickTree, node1: String, node2: String): Int = {
    val oneOfTheNodes: String = s"($node1|$node2)"
    val Expression: Regex = (oneOfTheNodes + "(.*?)" + oneOfTheNodes).r
    val charactersBetweenNodes: String = Expression.findFirstIn(newickTree).get

    def countParens(acc: (Int, Int), character: Char): (Int, Int) = {
      val (up, down): (Int, Int) = acc
      val (modifiedUp, modifiedDown): (Int, Int) = {
        if (character == ',' || character == ')') if (down > 0) (up, down - 1) else (up + 1, down)
        else (up, down)
      }
      if (character == ',' || character == '(') (modifiedUp, modifiedDown + 1)
      else (modifiedUp, modifiedDown)
    }

    val (upward, downward): (Int, Int) = charactersBetweenNodes.foldLeft((0, 0))(countParens)
    upward + downward
  }

  def calcDistancesInNewickTrees(treesAndNodePairs: List[(NewickTree, NodePair)]): List[Int] =
    treesAndNodePairs.map { case (newickTree, (node1, node2)) => calcDistanceBetweenNodes(newickTree, node1, node2) }

  def main(args: Array[String]): Unit = {
    val treesAndNodePairs: List[(NewickTree, NodePair)] = getData(isPractice = false)
    val result: List[Int] = calcDistancesInNewickTrees(treesAndNodePairs)
    writeListAsStringToFile(result)
  }
}
