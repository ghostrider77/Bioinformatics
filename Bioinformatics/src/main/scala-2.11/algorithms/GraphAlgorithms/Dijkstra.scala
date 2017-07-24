package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/dij/
  */

object Dijkstra {

  object SampleData {
    val sample: List[String] =
      List(
        "6 10",
        "3 4 4",
        "1 2 4",
        "1 3 2",
        "2 3 3",
        "6 3 2",
        "3 5 5",
        "5 4 1",
        "3 2 1",
        "2 4 2",
        "2 5 3"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}
  import utils.GraphUtilityFunctions.readWeightedEdgeList
  import algorithms.Datastructures.WeightedGraph.shortestPath
  import algorithms.Datastructures.{WeightedEdge, WeightedGraph}

  val inputFileName: String = "/algorithms/datasets/rosalind_dij.txt"

  def getData(isPractice: Boolean): WeightedGraph = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(numberOfNodes, _): List[Int] = convertStringToIntList(data.head)
    val edges: List[WeightedEdge] = readWeightedEdgeList(data.tail)
    WeightedGraph(numberOfNodes, edges, isDirected = true)
  }

  def main(args: Array[String]): Unit = {
    val graph: WeightedGraph = getData(isPractice = false)
    val result: List[Int] = shortestPath(graph, baseNode = 1)
    writeListAsStringToFile(result)
  }

}
