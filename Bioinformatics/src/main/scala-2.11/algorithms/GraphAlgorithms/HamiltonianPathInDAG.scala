package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/hdag/
  */

object HamiltonianPathInDAG {

  object SampleData {
    val sample: List[String] =
      List(
        "2",
        "3 3",
        "1 2",
        "2 3",
        "1 3",
        "4 3",
        "4 3",
        "3 2",
        "4 1"
      )
  }

  import scala.annotation.tailrec
  import algorithms.Datastructures.Graph
  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}
  import utils.GraphUtilityFunctions.{Node, readListOfGraphs}

  val inputFileName: String = "/algorithms/datasets/rosalind_hdag.txt"

  def getData(isPractice: Boolean): List[Graph] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readListOfGraphs(data, isDirected = true, casesSeparatedByEmptyLine = false)
  }

  def findHamiltonianPath(graph: Graph): Option[List[Node]] = {
    val topologicalOrder: List[Node] = graph.topologicalSorting.get

    @tailrec
    def loop(consecutiveNodes: List[(Node, Node)]): Option[List[Node]] = {
      if (consecutiveNodes.isEmpty) Some(topologicalOrder)
      else {
        val (node1, node2): (Node, Node) = consecutiveNodes.head
        if (!graph.adjacencyList.getOrElse(node1, Nil).contains(node2)) None
        else loop(consecutiveNodes.tail)
      }
    }

    loop(topologicalOrder.zip(topologicalOrder.tail))
  }

  def main(args: Array[String]): Unit = {
    val graphs: List[Graph] = getData(isPractice = false)
    val result: List[List[Node]] =
      graphs.map(findHamiltonianPath).map(path => if (path.isEmpty) List(-1) else 1 :: path.get)
    writeListOfListsAsStringsToFile(result)
  }

}
