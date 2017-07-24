package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/bf/
  */

object BellmanFord {

  object SampleData {
    val sample: List[String] =
      List(
        "9 13",
        "1 2 10",
        "3 2 1",
        "3 4 1",
        "4 5 3",
        "5 6 -1",
        "7 6 -1",
        "8 7 1",
        "1 8 8",
        "7 2 -4",
        "2 6 2",
        "6 3 -2",
        "9 5 -10",
        "9 4 7"
      )
  }

  import SampleData.sample
  import utils.GraphUtilityFunctions.{costToString, readWeightedEdgeList}
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}
  import algorithms.Datastructures.WeightedGraph.shortestPathFord
  import algorithms.Datastructures.{WeightedEdge, WeightedGraph}

  val inputFileName: String = "/algorithms/datasets/rosalind_bf.txt"

  def getData(isPractice: Boolean): WeightedGraph = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(numberOfNodes, _): List[Int] = convertStringToIntList(data.head)
    val edges: List[WeightedEdge] = readWeightedEdgeList(data.tail)
    WeightedGraph(numberOfNodes, edges, isDirected = true)
  }

  def main(args: Array[String]): Unit = {
    val graph: WeightedGraph = getData(isPractice = false)
    val result: List[String] = shortestPathFord(graph, baseNode = 1).map(costToString)
    writeListAsStringToFile(result)
  }

}
