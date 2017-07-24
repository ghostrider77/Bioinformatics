package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/ts/
  */

object TopologicalSorting {

  object SampleData {
    val sample: List[String] =
      List(
        "4 5",
        "1 2",
        "3 1",
        "3 2",
        "4 3",
        "4 2"
      )
  }

  import algorithms.Datastructures.Graph
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}
  import utils.GraphUtilityFunctions.{Edge, Node, readEdgeList}

  val inputFileName: String = "/algorithms/datasets/rosalind_ts.txt"

  def getData(isPractice: Boolean): Graph = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(numberOfNodes, _): List[Int] = convertStringToIntList(data.head)
    val edges: List[Edge] = readEdgeList(data.tail)
    Graph(numberOfNodes, edges, isDirected = true)
  }

  def main(args: Array[String]): Unit = {
    val graph: Graph = getData(isPractice = false)
    val result: List[Node] = graph.topologicalSorting.get
    writeListAsStringToFile(result)
  }

}
