package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/cte/
  */

object ShortestCycleThroughGivenEdge {

  object SampleData {
    val sample: List[String] =
      List(
        "2",
        "4 5",
        "2 4 2",
        "3 2 1",
        "1 4 3",
        "2 1 10",
        "1 3 4",
        "4 5",
        "3 2 1",
        "2 4 2",
        "4 1 3",
        "2 1 10",
        "1 3 4"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}
  import utils.GraphUtilityFunctions.readNonSeparatedListOfWeightedEdgeLists
  import algorithms.Datastructures.WeightedGraph.shortestPath
  import algorithms.Datastructures.{WeightedEdge, WeightedGraph}

  val inputFileName: String = "/algorithms/datasets/rosalind_cte.txt"

  def getData(isPractice: Boolean): List[WeightedGraph] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readNonSeparatedListOfWeightedEdgeLists(data, isDirected = true)
  }

  def calcShortestCycleThroughEdge(graph: WeightedGraph, edge: WeightedEdge): Int = {
    val shortestDistances: List[Int] = shortestPath(graph, edge.node2)
    val distanceToEdgeStartNode: Int = shortestDistances(edge.node1 - 1)
    if (distanceToEdgeStartNode == -1) -1 else distanceToEdgeStartNode + edge.weight
  }

  def shortestCycleThroughGivenEdge(graphs: List[WeightedGraph]): List[Int] =
    for { graph <- graphs } yield {
      val edge: WeightedEdge = graph.edgeList.head
      calcShortestCycleThroughEdge(graph, edge)
    }

  def main(args: Array[String]): Unit = {
    val graphs: List[WeightedGraph] = getData(isPractice = false)
    val result: List[Int] = shortestCycleThroughGivenEdge(graphs)
    writeListAsStringToFile(result)
  }

}
