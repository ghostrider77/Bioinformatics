package algorithms.Datastructures

import utils.GraphUtilityFunctions.{Node, Weight}

import scala.annotation.tailrec

case class WeightedEdge(node1: Node, node2: Node, weight: Weight)

case class WeightedGraph(numberOfNodes: Int,
                         edgeList: List[WeightedEdge],
                         isDirected: Boolean) {

  lazy val adjacencyList: Map[Node, List[(Node, Weight)]] = createAdjacencyList()
  private lazy val areEdgeWeightsNonNegative: Boolean = edgeList.forall(_.weight >= 0)

  private[this] def createAdjacencyList(): Map[Node, List[(Node, Weight)]] = {
    val directedEdges: List[WeightedEdge] =
      if (this.isDirected) edgeList
      else edgeList.flatMap{ case edge @ WeightedEdge(n1, n2, w) => List(edge, WeightedEdge(n2, n1, w)) }
    directedEdges
      .groupBy(_.node1)
      .mapValues(weightedEdgesFromNode => weightedEdgesFromNode.map{ case WeightedEdge(_, n2, w) => (n2, w) })
  }

}

object WeightedGraph {
  import collection.mutable.{Set => MutableSet}

  private def updateShortestDistances(adjacencyList: Map[Node, List[(Node, Weight)]],
                                      currentBaseNode: Node,
                                      shortestDistances: Array[Double]): Unit = {
    val neighbours: List[(Node, Weight)] = adjacencyList.getOrElse(currentBaseNode, Nil)
    val shortestDistanceToNode: Double = shortestDistances(currentBaseNode - 1)
    for { (neighbour, weight) <- neighbours } {
      val distanceThroughNode: Double = shortestDistanceToNode + weight
      if (shortestDistances(neighbour - 1) > distanceThroughNode)
        shortestDistances(neighbour - 1) = distanceThroughNode
    }
  }

  private def selectNodeInTWithShortestDistance(shortestDistances: Array[Double], nodesinT: MutableSet[Node]): Node = {
    val shortestDistancesInT: Array[(Double, Int)] =
      shortestDistances.zipWithIndex.filter{ case (_, ix) => nodesinT.contains(ix + 1) }
    shortestDistancesInT.minBy(_._1)._2 + 1
  }

  def shortestPath(graph: WeightedGraph, baseNode: Node): List[Int] = {
    require(graph.areEdgeWeightsNonNegative, "There are negative edge weights, use Ford's algorithm instead")

    val shortestDistances: Array[Double] = Array.fill(graph.numberOfNodes)(Double.PositiveInfinity)
    shortestDistances(baseNode - 1) = 0
    val nodesinT: MutableSet[Node] =
      MutableSet.empty[Node] ++ (1 to graph.numberOfNodes).filterNot(_ == baseNode).toSet

    @tailrec
    def loop(currentBaseNode: Node): Unit = {
      if (nodesinT.nonEmpty) {
        updateShortestDistances(graph.adjacencyList, currentBaseNode, shortestDistances)
        val node: Node = selectNodeInTWithShortestDistance(shortestDistances, nodesinT)
        nodesinT.remove(node)
        loop(node)
      }
    }

    loop(baseNode)
    shortestDistances.map(dist => if (dist.isPosInfinity) -1 else dist.toInt).toList
  }

  def shortestPathFord(graph: WeightedGraph, baseNode: Node): List[Double] = {
    val shortestDistances: Array[Double] = Array.fill(graph.numberOfNodes)(Double.PositiveInfinity)
    shortestDistances(baseNode - 1) = 0

    for {
      _ <- 1 to graph.numberOfNodes
      node <- graph.adjacencyList.keysIterator
    } updateShortestDistances(graph.adjacencyList, node, shortestDistances)

    shortestDistances.toList
  }
}
