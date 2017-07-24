package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/nwc/
  */

object NegativeWeightCycle {

  object SampleData {
    val sample: List[String] =
      List(
        "2",
        "4 5",
        "1 4 4",
        "4 2 3",
        "2 3 1",
        "3 1 6",
        "2 1 -7",
        "3 4",
        "1 2 -8",
        "2 3 20",
        "3 1 -1",
        "3 2 -30"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import algorithms.Datastructures.{WeightedEdge, WeightedGraph}
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}
  import utils.GraphUtilityFunctions.readNonSeparatedListOfWeightedEdgeLists

  val inputFileName: String = "/algorithms/datasets/rosalind_nwc.txt"

  def getData(isPractice: Boolean): List[WeightedGraph] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readNonSeparatedListOfWeightedEdgeLists(data, isDirected = true)
  }

  def updateShortestDistances(shortestDistances: Array[Double], edgeList: List[WeightedEdge]): Boolean = {

    @tailrec
    def loop(edges: List[WeightedEdge], isSomeNodeUpdated: Boolean): Boolean = {
      if (edges.isEmpty) isSomeNodeUpdated
      else {
        val WeightedEdge(node, neighbour, weight) = edges.head
        val distanceThroughNode: Double = shortestDistances(node - 1) + weight
        if (shortestDistances(neighbour - 1) > distanceThroughNode) {
          shortestDistances(neighbour - 1) = distanceThroughNode
          loop(edges.tail, isSomeNodeUpdated = true)
        }
        else loop(edges.tail, isSomeNodeUpdated)
      }
    }

    loop(edgeList, isSomeNodeUpdated = false)
  }

  def hasGraphNegativeCycle(graph: WeightedGraph): Boolean = {
    val shortestDistances: Array[Double] = Array.fill(graph.numberOfNodes)(0.0)

    @tailrec
    def loop(passOnEdges: Int): Boolean = {
      if (passOnEdges > graph.numberOfNodes) false
      else {
        val isSomeNodeUpdated: Boolean = updateShortestDistances(shortestDistances, graph.edgeList)
        if (passOnEdges == graph.numberOfNodes && isSomeNodeUpdated) true
        else loop(passOnEdges + 1)
      }
    }

    loop(1)
  }

  def detectNegativeCycles(graphs: List[WeightedGraph]): List[Boolean] =
    for { graph <- graphs } yield hasGraphNegativeCycle(graph)

  def main(args: Array[String]): Unit = {
    val graphs: List[WeightedGraph] = getData(isPractice = false)
    val result: List[Int] = detectNegativeCycles(graphs).map(if (_) 1 else -1)
    writeListAsStringToFile(result)
  }

}
