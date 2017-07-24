package utils

import algorithms.Datastructures.{Graph, WeightedEdge, WeightedGraph}

object GraphUtilityFunctions {
  type Node = Int
  type Edge = (Node, Node)
  type Weight = Int
  type Component = Set[Node]

  def readEdgeList(lines: List[String]): List[Edge] = {
    for { line <- lines } yield {
      val Array(nodeA, nodeB): Array[Int] = line.split(" ").map(_.toInt)
      (nodeA, nodeB)
    }
  }

  def readWeightedEdgeList(lines: List[String]): List[WeightedEdge] = {
    for { line <- lines } yield {
      val Array(nodeA, nodeB, weight): Array[Int] = line.split(" ").map(_.toInt)
      WeightedEdge(nodeA, nodeB, weight)
    }
  }

  def readNonSeparatedListOfWeightedEdgeLists(lines: List[String], isDirected: Boolean): List[WeightedGraph] = {
    val reader: Iterator[String] = lines.toIterator
    val numberOfTestCases: Int = reader.next().toInt

    (for { _ <- 0 until numberOfTestCases } yield {
      val Array(numberOfNodes, numberOfEdges): Array[Int] = reader.next().split(" ").map(_.toInt)
      val edges: List[WeightedEdge] = (for { _ <- 0 until numberOfEdges } yield {
        val Array(node1, node2, weight): Array[Int] = reader.next().split(" ").map(_.toInt)
        WeightedEdge(node1, node2, weight)
      }).toList
      WeightedGraph(numberOfNodes, edges, isDirected)
    }).toList
  }

  def readListOfGraphs(lines: List[String],
                       isDirected: Boolean,
                       casesSeparatedByEmptyLine: Boolean = true): List[Graph] = {
    val reader: Iterator[String] = lines.toIterator
    val numberOfTestCases: Int = reader.next().toInt

    (for { _ <- 0 until numberOfTestCases } yield {
      if (casesSeparatedByEmptyLine) reader.next()
      val Array(numberOfNodes, numberOfEdges): Array[Int] = reader.next().split(" ").map(_.toInt)
      val edges: List[Edge] = (for { _ <- 0 until numberOfEdges } yield {
        val Array(node1, node2): Array[Int] = reader.next().split(" ").map(_.toInt)
        (node1, node2)
      }).toList
      Graph(numberOfNodes, edges, isDirected)
    }).toList
  }

  def costToString(cost: Double): String = if (cost.isPosInfinity) "x" else cost.toInt.toString
}
