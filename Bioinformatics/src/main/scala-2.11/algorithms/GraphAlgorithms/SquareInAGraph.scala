package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/sq/
  */

object SquareInAGraph {

  object SampleData {
    val sample: List[String] =
      List(
        "2",
        "",
        "4 5",
        "3 4",
        "4 2",
        "3 2",
        "3 1",
        "1 2",
        "",
        "4 4",
        "1 2",
        "3 4",
        "2 4",
        "4 1"
      )
  }

  import scala.annotation.tailrec
  import collection.mutable.{Set => MutableSet}
  import SampleData.sample
  import algorithms.Datastructures.Graph
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}
  import utils.GraphUtilityFunctions.{Node, readListOfGraphs}

  val inputFileName: String = "/algorithms/datasets/rosalind_sq.txt"

  def getData(isPractice: Boolean): List[Graph] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readListOfGraphs(data, isDirected = false)
  }

  def checkNodesWithCommonNeighbour(neighbours: List[Node],
                                    nodePairsWithCommonNeighbour: MutableSet[(Node, Node)]): Boolean = {
    val neighbourPairs: Iterator[List[Node]] = neighbours.combinations(2)

    @tailrec
    def loop: Boolean = {
      if (!neighbourPairs.hasNext) false
      else {
        val List(node1, node2): List[Node] = neighbourPairs.next()
        val (smallerNode, largerNode): (Node, Node) = if (node1 < node2) (node1, node2) else (node2, node1)
        if (nodePairsWithCommonNeighbour.contains((smallerNode, largerNode))) true
        else {
          nodePairsWithCommonNeighbour += ((smallerNode, largerNode))
          loop
        }
      }
    }

    loop
  }

  def hasCycleWithLengthFour(graph: Graph): Boolean = {
    val nodePairsWithCommonNeighbour: MutableSet[(Node, Node)] = MutableSet()
    val allNodesAsNeighbours: Iterator[List[Node]] = graph.adjacencyList.valuesIterator

    @tailrec
    def loop: Boolean = {
      if (!allNodesAsNeighbours.hasNext) false
      else {
        val neighbours: List[Node] = allNodesAsNeighbours.next()
        val hasCycleFound: Boolean = checkNodesWithCommonNeighbour(neighbours, nodePairsWithCommonNeighbour)
        if (hasCycleFound) hasCycleFound
        else loop
      }
    }

    loop
  }

  def findCyclesWithLengthFour(graphs: List[Graph]): List[Boolean] = graphs.map(hasCycleWithLengthFour)

  def main(args: Array[String]): Unit = {
    val graphs: List[Graph] = getData(isPractice = false)
    val result: List[Int] = findCyclesWithLengthFour(graphs).map(if (_) 1 else -1)
    writeListAsStringToFile(result)
  }

}
