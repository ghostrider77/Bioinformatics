package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/sdag/
  */

object ShortestPathsInDAG {

  object SampleData {
    val sample: List[String] =
      List(
        "5 6",
        "2 3 4",
        "4 3 -2",
        "1 4 1",
        "1 5 -3",
        "2 4 -2",
        "5 4 1"
      )
  }

  import SampleData.sample
  import utils.GraphUtilityFunctions.{Edge, Node, Weight, readWeightedEdgeList, costToString}
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}
  import algorithms.Datastructures.{Graph, WeightedEdge, WeightedGraph}

  val inputFileName: String = "/algorithms/datasets/rosalind_sdag.txt"

  def getData(isPractice: Boolean): WeightedGraph = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(numberOfNodes, _): List[Int] = convertStringToIntList(data.head)
    val edges: List[WeightedEdge] = readWeightedEdgeList(data.tail)
    WeightedGraph(numberOfNodes, edges, isDirected = true)
  }

  def calcTopologicalSorting(weightedGraph: WeightedGraph): List[Node] = {
    val edges: List[Edge] = weightedGraph.edgeList.map{ case WeightedEdge(node1, node2, _) => (node1, node2) }
    Graph(weightedGraph.numberOfNodes, edges, isDirected = weightedGraph.isDirected).topologicalSorting.get
  }

  def findShortestPaths(weightedGraph: WeightedGraph, baseNode: Node): List[Double] = {
    val topologicalOrder: List[Node] = calcTopologicalSorting(weightedGraph)
    val shortestDistances: Array[Double] = Array.fill(weightedGraph.numberOfNodes)(Double.PositiveInfinity)
    shortestDistances(baseNode - 1) = 0

    for {
      node <- topologicalOrder
      neighbours: List[(Node, Weight)] = weightedGraph.adjacencyList.getOrElse(node, Nil)
      (neighbour, weight) <- neighbours
    } {
      val distanceThroughNode: Double = shortestDistances(node - 1) + weight
      if (shortestDistances(neighbour - 1) > distanceThroughNode)
        shortestDistances(neighbour - 1) = distanceThroughNode
    }

    shortestDistances.toList
  }

  def main(args: Array[String]): Unit = {
    val weightedGraph: WeightedGraph = getData(isPractice = false)
    val result: List[String] = findShortestPaths(weightedGraph, baseNode = 1).map(costToString)
    writeListAsStringToFile(result)
  }

}
