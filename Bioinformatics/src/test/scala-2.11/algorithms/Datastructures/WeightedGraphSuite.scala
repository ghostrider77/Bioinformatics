package algorithms.Datastructures

import org.scalatest.{FreeSpec, Matchers}

class WeightedGraphSuite extends FreeSpec with Matchers {

  object TestGraphs {
    private val numberOfNodes: Int = 9
    private val nonnegativeEdges: List[WeightedEdge] =
      List(
        WeightedEdge(node1 = 1, node2 = 3, weight = 1),
        WeightedEdge(node1 = 1, node2 = 4, weight = 2),
        WeightedEdge(node1 = 2, node2 = 1, weight = 4),
        WeightedEdge(node1 = 2, node2 = 7, weight = 1),
        WeightedEdge(node1 = 3, node2 = 9, weight = 6),
        WeightedEdge(node1 = 4, node2 = 3, weight = 4),
        WeightedEdge(node1 = 4, node2 = 5, weight = 3),
        WeightedEdge(node1 = 5, node2 = 1, weight = 1),
        WeightedEdge(node1 = 6, node2 = 1, weight = 1),
        WeightedEdge(node1 = 6, node2 = 2, weight = 1),
        WeightedEdge(node1 = 7, node2 = 6, weight = 1)
      )
    private val negativeEdges: List[WeightedEdge] =
      List(
        WeightedEdge(node1 = 1, node2 = 3, weight = 1),
        WeightedEdge(node1 = 1, node2 = 4, weight = 2),
        WeightedEdge(node1 = 2, node2 = 1, weight = 4),
        WeightedEdge(node1 = 2, node2 = 7, weight = 1),
        WeightedEdge(node1 = 3, node2 = 9, weight = 6),
        WeightedEdge(node1 = 4, node2 = 3, weight = -4),
        WeightedEdge(node1 = 4, node2 = 5, weight = 3),
        WeightedEdge(node1 = 5, node2 = 1, weight = -1),
        WeightedEdge(node1 = 6, node2 = 1, weight = 10),
        WeightedEdge(node1 = 6, node2 = 2, weight = 2),
        WeightedEdge(node1 = 7, node2 = 6, weight = -2)
      )
    val graph: WeightedGraph = WeightedGraph(numberOfNodes, nonnegativeEdges, isDirected = true)
    val negativeGraph: WeightedGraph = WeightedGraph(numberOfNodes, negativeEdges, isDirected = true)
  }

  "Dijkstra's shortest path algorithm" - {
    import TestGraphs.graph
    import WeightedGraph.shortestPath

    "should find the shortest paths from the starting node in a graph with positive edge weights" in {
      shortestPath(graph, baseNode = 1) shouldEqual List(0, -1, 1, 2, 5, -1, -1, -1, 7)
      shortestPath(graph, baseNode = 8) shouldEqual List(-1, -1, -1, -1, -1, -1, -1, 0, -1)
      shortestPath(graph, baseNode = 2) shouldEqual List(3, 0, 4, 5, 8, 2, 1, -1, 10)
    }
  }

  "Ford's shortest path algorithm" - {
    import TestGraphs.negativeGraph
    import WeightedGraph.shortestPathFord
    import utils.GraphUtilityFunctions.costToString

    "should find the shortest paths from the starting node in a graph that does not contain negative cycles" in {
      shortestPathFord(negativeGraph, baseNode = 1).map(costToString) shouldEqual
        List("0", "x", "-2", "2", "5", "x", "x", "x", "4")
      shortestPathFord(negativeGraph, baseNode = 8).map(costToString) shouldEqual
        List("x", "x", "x", "x", "x", "x", "x", "0", "x")
      shortestPathFord(negativeGraph, baseNode = 2).map(costToString) shouldEqual
        List("4", "0", "2", "6", "9", "-1", "1", "x", "8")
    }
  }

}
