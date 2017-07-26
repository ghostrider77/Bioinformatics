package algorithms.Datastructures

import org.scalatest.{FreeSpec, Matchers}

class GraphSuite extends FreeSpec with Matchers {
  import utils.GraphUtilityFunctions.{Edge, Node}

  object TestGraphs {

    def isTopologicalSortingValid(adjacencyList: Map[Node, List[Node]], orderedNodes: List[Node]): Boolean = {
      adjacencyList.forall { case (node, neighbours) =>
        val indexOfNode: Int = orderedNodes.indexOf(node)
        neighbours.forall { neighbour => orderedNodes.indexOf(neighbour) > indexOfNode }
      }
    }

    private val numberOfNodes: Int = 9
    private val edges: List[Edge] =
      List(
        (1, 3),
        (1, 4),
        (2, 1),
        (2, 7),
        (3, 9),
        (4, 3),
        (4, 5),
        (5, 1),
        (6, 1),
        (6, 2),
        (7, 6)
      )
    val directedGraph: Graph = Graph(numberOfNodes, edges, isDirected = true)
    val undirectedGraph: Graph = Graph(numberOfNodes, edges, isDirected = false)

    val directedAcyclicGraph: Graph =
      Graph(numberOfNodes = 5, edgeList = List((1, 4), (2, 1), (2, 3), (2, 4), (3, 5), (4, 5)), isDirected = true)
  }

  "Graph" - {
    import TestGraphs.{directedGraph, undirectedGraph}

    "should have the correct nodes in the connected components for undirected graphs" in {
      undirectedGraph.connectedComponents shouldBe defined
      undirectedGraph.connectedComponents.get.toSet shouldEqual Set(Set(1, 2, 3, 4, 5, 6, 7, 9), Set(8))
    }

    "should not have connected components defined for directed graphs" in {
      directedGraph.connectedComponents shouldBe None
    }

  }

  "BreadthFirstSearch" - {
    import TestGraphs.{directedGraph, undirectedGraph}
    import algorithms.Datastructures.Graph.breadthFirstSearch

    "should calculate shortest distances from starting node in a directed graph" in {
      breadthFirstSearch(directedGraph, startNode = 1) shouldEqual List(0, -1, 1, 1, 2, -1, -1, -1, 2)
      breadthFirstSearch(directedGraph, startNode = 8) shouldEqual List(-1, -1, -1, -1, -1, -1, -1, 0, -1)
      breadthFirstSearch(directedGraph, startNode = 2) shouldEqual List(1, 0, 2, 2, 3, 2, 1, -1, 3)
    }

    "should calculate shortest distances from starting node in an undirected graph" in {
      breadthFirstSearch(undirectedGraph, startNode = 1) shouldEqual List(0, 1, 1, 1, 1, 1, 2, -1, 2)
      breadthFirstSearch(undirectedGraph, startNode = 8) shouldEqual List(-1, -1, -1, -1, -1, -1, -1, 0, -1)
      breadthFirstSearch(undirectedGraph, startNode = 2) shouldEqual List(1, 0, 2, 2, 2, 1, 1, -1, 3)
    }
  }

  "TopologicalSorting" - {
    import TestGraphs.{directedAcyclicGraph, isTopologicalSortingValid}

    "should return None if the graph is not acyclic" in {
      val graph: Graph = Graph(numberOfNodes = 3, edgeList = List((1, 2), (2, 3), (3, 1)), isDirected = true)
      graph.topologicalSorting shouldBe None
    }

    "should retrieve a valid topological sorting of the nodes when the graph is a DAG" in {
      directedAcyclicGraph.topologicalSorting shouldBe defined
      val topologicalSorting: List[Node] = directedAcyclicGraph.topologicalSorting.get
      isTopologicalSortingValid(directedAcyclicGraph.adjacencyList, topologicalSorting) shouldBe true
    }
  }

  "StronglyConnectedComponents" - {
    import TestGraphs.{directedGraph, undirectedGraph}

    "should calculate the strongly connected components for directed graphs" in {
      directedGraph.stronglyConnectedComponents shouldBe defined
      directedGraph.stronglyConnectedComponents.get.toSet shouldEqual
        Set(Set(2, 6, 7), Set(1, 4, 5), Set(3), Set(9), Set(8))
    }

    "should not calculate strongly connected components for undirected graphs" in {
      undirectedGraph.stronglyConnectedComponents shouldBe None
    }
  }

}
