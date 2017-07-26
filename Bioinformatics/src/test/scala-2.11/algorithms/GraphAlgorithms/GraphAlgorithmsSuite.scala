package algorithms.GraphAlgorithms

import org.scalatest.{FreeSpec, Matchers}

class GraphAlgorithmsSuite extends FreeSpec with Matchers {
  import algorithms.Datastructures.{Graph, WeightedGraph}
  import utils.GraphUtilityFunctions.{Edge, Node, Component}

  "DegreeArray" - {
    import DegreeArray.{calcOutDegrees, getData}

    "should solve the sample problem correctly" in {
      val (n, edgeList): (Int, List[Edge]) = getData(isPractice = true)
      val degrees: Map[Node, Int] = calcOutDegrees(edgeList)
      val result: List[Int] = (1 to n).map(k => degrees.getOrElse(k, 0)).toList
      result shouldEqual List(2, 4, 2, 2, 2, 2)
    }

  }

  "DoubleDegreeArray" - {
    import DoubleDegreeArray.{getData, calcTotalOutDegreesOfNeighbours}

    "should calculate degrees of neighbours for the sample problem correctly" in {
      val (n, edgeList): (Int, List[Edge]) = getData(isPractice = true)
      calcTotalOutDegreesOfNeighbours(edgeList, n) shouldEqual List(3, 5, 5, 5, 0)
    }
  }

  "BreadthFirstSearch" - {
    import BreadthFirstSearch.getData
    import Graph.breadthFirstSearch

    "should calculate distances from stating node correctly" in {
      val graph: Graph = getData(isPractice = true)
      breadthFirstSearch(graph, startNode = 1) shouldEqual List(0, -1, 2, 1, 3, 2)
    }
  }

  "ConnectedComponents" - {
    import ConnectedComponents.getData

    "should calculate the number of connected components for the sample problem correctly" in {
      val graph: Graph = getData(isPractice = true)
      graph.connectedComponents.get should have length 3
    }
  }

  "TestingBipartiteness" - {
    import TestingBipartiteness.{getData, testForBipartiteness, isBipartite}

    "should detect bipartiteness for the sample problems" in {
      val graphs: List[Graph] = getData(isPractice = true)
      testForBipartiteness(graphs) shouldEqual List(false, true)
    }

    "should correctly detect bipartite graphs" in {
      val numberOfNodes: Int = 6
      val edges: List[Edge] = List((1, 2), (1, 3), (3, 6), (3, 5))
      val bipartiteGraph: Graph = Graph(numberOfNodes, edges, isDirected = false)
      isBipartite(bipartiteGraph) shouldBe true
    }
  }

  "TestingAcyclicity" - {
    import TestingAcyclicity.getData

    "should detect graphs not containing cycles for the sample problem" in {
      val graphs: List[Graph] = getData(isPractice = true)
      graphs.map(_.isGraphDAG) shouldEqual List(true, false, true)
    }

    "should only detect directed cycles" in {
      val graph1: Graph = Graph(numberOfNodes = 3, edgeList = List((2, 1), (3, 2), (3, 1)), isDirected = true)
      val graph2: Graph =
        Graph(numberOfNodes = 4, edgeList = List((2, 1), (2, 3), (2, 4), (3, 1), (4, 3)), isDirected = true)
      graph1.isGraphDAG shouldBe true
      graph2.isGraphDAG shouldBe true
    }

    "should detect directed cycle in graph containing a cycle" in {
      val graph: Graph =
        Graph(numberOfNodes = 4, edgeList = List((2, 1), (2, 4), (3, 1), (3, 2), (4, 3)), isDirected = true)
      graph.isGraphDAG shouldBe false
    }
  }

  "Dijkstra's Algorithm" - {
    import Dijkstra.getData
    import algorithms.Datastructures.WeightedGraph.shortestPath

    "should calculate the shortest distances from the first node" in {
      val graph: WeightedGraph = getData(isPractice = true)
      shortestPath(graph, baseNode = 1) shouldEqual List(0, 3, 2, 5, 6, -1)
    }
  }

  "SquareInGraph" - {
    import SquareInAGraph.{getData, findCyclesWithLengthFour}

    "should detect graphs that have cycles of length 4" in {
      val graphs: List[Graph] = getData(isPractice = true)
      findCyclesWithLengthFour(graphs) shouldEqual List(true, false)
    }
  }

  "BellmanFord" - {
    import BellmanFord.getData
    import utils.GraphUtilityFunctions.costToString
    import WeightedGraph.shortestPathFord

    "should calculate shortest paths from starting node when the graph has negative edges but no negative cycles" in {
      val graph: WeightedGraph = getData(isPractice = true)
      shortestPathFord(graph, baseNode = 1).map(costToString) shouldEqual
        List("0", "5", "5", "6", "9", "7", "9", "8", "x")
    }
  }

  "ShortestCycleThroughGivenEdge" - {
    import ShortestCycleThroughGivenEdge.{getData, shortestCycleThroughGivenEdge}

    "should calculate the shortest cycle through a given edge for the sample problems" in {
      val graphs: List[WeightedGraph] = getData(isPractice = true)
      shortestCycleThroughGivenEdge(graphs) shouldEqual List(-1, 10)
    }
  }

  "TopologicalSorting" - {
    import TopologicalSorting.getData

    "should retrieve the topological order of nodes in a directed acyclic graph" in {
      val graph: Graph = getData(isPractice = true)
      graph.topologicalSorting.get shouldEqual List(4, 3, 1, 2)
    }
  }

  "HamiltonianPathInDAG" - {
    import HamiltonianPathInDAG.{getData, findHamiltonianPath}

    "should find Hamiltonian path in a DAG for the sample problems" in {
      val graphs: List[Graph] = getData(isPractice = true)
      val paths: List[Option[List[Node]]] = graphs.map(findHamiltonianPath)
      paths.head shouldBe defined
      paths.head.get shouldEqual List(1, 2, 3)
      paths(1) shouldBe None
    }

    "should return None if there is no Hamiltonian path in a DAG" in {
      val graph: Graph = Graph(numberOfNodes = 4, edgeList = List((1, 2), (3, 1), (3, 2), (4, 1)), isDirected = true)
      findHamiltonianPath(graph) shouldBe None
    }
  }

  "NegativeWeightCycle" - {
    import NegativeWeightCycle.{getData, hasGraphNegativeCycle}

    "should detect the presence of negative weight cycles for the sample problems" in {
      val graphs: List[WeightedGraph] = getData(isPractice = true)
      graphs.map(hasGraphNegativeCycle) shouldEqual List(false, true)
    }
  }

  "StronglyConnectedComponents" - {
    import StronglyConnectedComponents.getData

    "should calculate the number of strongly connected components of the sample problem" in {
      val graph: Graph = getData(isPractice = true)
      graph.stronglyConnectedComponents shouldBe defined
      graph.stronglyConnectedComponents.get should have length 3
    }
  }

  "GeneralSink" - {
    import GeneralSink.{getData, findSourceInDirectedGraph}

    "should return a source node from which all nodes are reachable via a directed path" in {
      val graphs: List[Graph] = getData(isPractice = true)
      graphs.map(findSourceInDirectedGraph) shouldEqual List(3, -1)
    }
  }

  "SemiConnectedGraph" - {
    import SemiConnectedGraph.{getData, isGraphSemiConnected}

    "should recognize semiconnected directed graphs in the sample problem" in {
      val graphs: List[Graph] = getData(isPractice = true)
      graphs.map(isGraphSemiConnected) shouldEqual List(true, false)
    }
  }

  "ShortestPathsInDAG" - {
    import ShortestPathsInDAG.{getData, findShortestPaths}
    import utils.GraphUtilityFunctions.costToString

    "should calculate shortest paths for directed acyclic graphs from a given base node" in {
      val weightedGraph: WeightedGraph = getData(isPractice = true)
      findShortestPaths(weightedGraph, baseNode = 1).map(costToString) shouldEqual List("0", "x", "-4", "-2", "-3")
    }
  }

  "TwoSatisfiability" - {
    import scala.collection.mutable.{Map => MutableMap}
    import TwoSatisfiability.{getData, isComponentContradictory, isCNFSatisfiable}

    "isComponentContradictory" - {

      "should return true when a component contains a literal and its negate as well" in {
        val numberOfVariables: Int = 4
        val component: Component = Set(2, 4, 5, 6)
        isComponentContradictory(component, numberOfVariables)._1 shouldBe true
      }

      "should return a logical assignment of the literals in a component when it is not contradictory" in {
        val numberOfVariables: Int = 4
        val component: Component = Set(2, 4, 5)
        val (isContradictory, assignment): (Boolean, MutableMap[Node, Boolean]) =
          isComponentContradictory(component, numberOfVariables)
        isContradictory shouldBe false
        assignment.toMap shouldEqual Map(2 -> true, 4 -> true, 1 -> false)
      }
    }

    "should return a valid logical assignment to a two-conjuctive-normal-form when possible" in {
      val cnfGraphs: List[Graph] = getData(isPractice = true)
      val result: List[List[Int]] = cnfGraphs.map(isCNFSatisfiable)
      result.head shouldEqual List(0)
      result(1).head shouldEqual 1
      result(1).tail should (equal (List(1, -2, 3)) or equal (List(-1, 2, -3)))
    }
  }

}
