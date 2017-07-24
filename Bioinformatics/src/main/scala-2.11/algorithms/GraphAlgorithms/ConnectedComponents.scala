package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/cc/
  */

object ConnectedComponents {

  object SampleData {
    val sample: List[String] =
      List(
        "12 13",
        "1 2",
        "1 5",
        "5 9",
        "5 10",
        "9 10",
        "3 4",
        "3 7",
        "3 8",
        "4 8",
        "7 11",
        "8 11",
        "11 12",
        "8 12"
      )
  }

  import algorithms.Datastructures.Graph
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}
  import utils.GraphUtilityFunctions.{Component, Edge, readEdgeList}

  val inputFileName: String = "/algorithms/datasets/rosalind_cc.txt"

  def getData(isPractice: Boolean): Graph = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(numberOfNodes, _): List[Int] = convertStringToIntList(data.head)
    val edges: List[Edge] = readEdgeList(data.tail)
    Graph(numberOfNodes, edges, isDirected = false)
  }

  def main(args: Array[String]): Unit = {
    val graph: Graph = getData(isPractice = false)
    val components: List[Component] = graph.connectedComponents.get
    val result: Int = components.length
    println(result)
  }

}
