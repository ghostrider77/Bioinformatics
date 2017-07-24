package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/bfs/
  */

object BreadthFirstSearch {

  object SampleData {
    val sample: List[String] =
      List(
        "6 6",
        "4 6",
        "6 5",
        "4 3",
        "3 5",
        "2 1",
        "1 4"
      )
  }

  import algorithms.Datastructures.Graph
  import algorithms.Datastructures.Graph.breadthFirstSearch
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}
  import utils.GraphUtilityFunctions.{Edge, readEdgeList}

  val inputFileName: String = "/algorithms/datasets/rosalind_bfs.txt"

  def getData(isPractice: Boolean): Graph = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(numberOfNodes, _): List[Int] = convertStringToIntList(data.head)
    val edges: List[Edge] = readEdgeList(data.tail)
    Graph(numberOfNodes, edges, isDirected = true)
  }

  def main(args: Array[String]): Unit = {
    val graph: Graph = getData(isPractice = false)
    val result: List[Int] = breadthFirstSearch(graph, startNode = 1)
    writeListAsStringToFile(result)
  }

}
