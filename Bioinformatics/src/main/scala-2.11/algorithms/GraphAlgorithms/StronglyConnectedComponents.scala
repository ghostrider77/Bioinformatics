package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/scc/
  */

object StronglyConnectedComponents {

  object SampleData {
    val sample: List[String] =
      List(
        "6 7",
        "4 1",
        "1 2",
        "2 4",
        "5 6",
        "3 2",
        "5 3",
        "3 5"
      )
  }

  import algorithms.Datastructures.Graph
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}
  import utils.GraphUtilityFunctions.{Component, Edge, readEdgeList}

  val inputFileName: String = "/algorithms/datasets/rosalind_scc.txt"

  def getData(isPractice: Boolean): Graph = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(numberOfNodes, _): List[Int] = convertStringToIntList(data.head)
    val edges: List[Edge] = readEdgeList(data.tail)
    Graph(numberOfNodes, edges, isDirected = true)
  }

  def main(args: Array[String]): Unit = {
    val graph: Graph = getData(isPractice = false)
    val components: List[Component] = graph.stronglyConnectedComponents.get
    val result: Int = components.length
    println(result)
  }

}
