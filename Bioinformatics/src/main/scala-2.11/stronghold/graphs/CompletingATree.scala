package stronghold.graphs

/**
  * problem description: http://rosalind.info/problems/tree/
  */

object CompletingATree {

  object SampleData {
    val sample: List[String] =
      List(
        "10",
        "1 2",
        "2 8",
        "4 10",
        "5 9",
        "6 10",
        "7 9"
      )
  }

  import SampleData.sample
  import algorithms.Datastructures.Graph
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}
  import utils.GraphUtilityFunctions.{Component, Edge, readEdgeList}

  val inputFileName: String = "/stronghold/datasets/rosalind_tree.txt"

  def getData(isPractice: Boolean): Graph = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(numberOfNodes): List[Int] = convertStringToIntList(data.head)
    val edges: List[Edge] = readEdgeList(data.tail)
    Graph(numberOfNodes, edges, isDirected = false)
  }

  def main(args: Array[String]): Unit = {
    val graph: Graph = getData(isPractice = false)
    val components: List[Component] = graph.connectedComponents.get
    println(components.length - 1)
  }

}