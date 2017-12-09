package algorithms.Datastructures

import scala.annotation.tailrec
import utils.GraphUtilityFunctions.{Component, Edge, Node}

case class DepthFirstSearchOutput(components: List[Component],
                                  visitStarted: Array[Int],
                                  visitEnded: Array[Int],
                                  topologicalSorting: List[Node])

case class Graph(numberOfNodes: Int,
                 edgeList: List[Edge],
                 isDirected: Boolean,
                 orderOfNodes: Option[List[Int]] = None) {
  import Graph.{breadthFirstSearch, createGraphWithEdgesReversed, depthFirstSearch, retrieveComponent}

  private lazy val dfsOutput: DepthFirstSearchOutput = depthFirstSearch(this)

  lazy val adjacencyList: Map[Node, List[Node]] = createAdjacencyList(this.edgeList)
  lazy val connectedComponents: Option[List[Component]] = calcConnectedComponents()
  lazy val stronglyConnectedComponents: Option[List[Component]] = calcStronglyConnectedComponents()
  lazy val topologicalSorting: Option[List[Node]] = if (this.isGraphDAG) Some(dfsOutput.topologicalSorting) else None

  val nodeOrder: List[Int] = getNodesInOrder(this.orderOfNodes)

  private[this] def createAdjacencyList(edgeList: List[Edge]): Map[Node, List[Node]] = {
    val directedEdges: List[Edge] = if (this.isDirected) edgeList else edgeList.flatMap(edge => List(edge, edge.swap))
    directedEdges.groupBy(_._1).mapValues(edgesFromNode => edgesFromNode.map(_._2))
  }

  private[this] def getNodesInOrder(nodes: Option[List[Node]]): List[Node] =
    if (nodes.nonEmpty) nodes.get else (1 to this.numberOfNodes).toList

  private[this] def calcConnectedComponents(): Option[List[Component]] = {

    @tailrec
    def loop(nodesNotInComponents: List[Node], components: List[Component]): List[Component] = {
      if (nodesNotInComponents.isEmpty) components
      else {
        val startNode: Node = nodesNotInComponents.head
        val distances: List[Int] = breadthFirstSearch(this, startNode)
        val currentComponent: Component = retrieveComponent(distances, this.numberOfNodes)
        val remainingNodes: List[Int] = nodesNotInComponents.filter(!currentComponent.contains(_))
        loop(remainingNodes, currentComponent :: components)
      }
    }

    if (this.isDirected) None
    else Some(loop((1 to this.numberOfNodes).toList, Nil))
  }

  private[this] def calcStronglyConnectedComponents(): Option[List[Component]] = {
    if (!this.isDirected) None
    else {
      val postVisitOrdering: Array[Int] = dfsOutput.visitEnded
      val reversedGraph: Graph = createGraphWithEdgesReversed(this, postVisitOrdering)
      Some(depthFirstSearch(reversedGraph).components)
    }
  }

  def isGraphDAG: Boolean = {
    if (!this.isDirected) false
    else {
      val postVisitOrdering: Array[Int] = dfsOutput.visitEnded
      this.adjacencyList.forall { case (node, neighbours) =>
        val nodeVisitEndNumber: Int = postVisitOrdering(node - 1)
        neighbours.forall { neighbour => nodeVisitEndNumber > postVisitOrdering(neighbour - 1) }
      }
    }
  }

}

object Graph {
  import scala.collection.mutable.{Map => MutableMap, Stack => MutableStack}

  private def getUnvisitedNeighbours(graph: Graph,
                                     baseNodes: List[Node],
                                     distancesFromStartNode: MutableMap[Node, Int]): List[Int] =
    for {
      node <- baseNodes
      neighbours: List[Node] = graph.adjacencyList.getOrElse(node, Nil)
      neighbour <- neighbours
      if !distancesFromStartNode.contains(neighbour)
    } yield neighbour

  private def addUnreachableNodes(graph: Graph, distancesFromStartNode: MutableMap[Node, Int]): List[Int] =
    (1 to graph.numberOfNodes).map(node => distancesFromStartNode.getOrElse(node, -1)).toList

  def breadthFirstSearch(graph: Graph, startNode: Node): List[Int] = {
    val distancesFromStartNode: MutableMap[Node, Int] = MutableMap.empty

    @tailrec
    def loop(nodes: List[Node], currentDistance: Int): Unit = {
      if (nodes.nonEmpty) {
        for { node <- nodes } distancesFromStartNode += node -> currentDistance
        val nodesOnNextLevel: List[Node] = getUnvisitedNeighbours(graph, nodes, distancesFromStartNode)
        loop(nodesOnNextLevel, currentDistance + 1)
      }
    }
    loop(List(startNode), currentDistance = 0)
    addUnreachableNodes(graph, distancesFromStartNode)
  }

  private def retrieveComponent(distancesFromStartNode: List[Int], numberOfNodes: Int): Component = {
    val distancesWithNodeIndex: List[(Int, Node)] = distancesFromStartNode.zip(1 to numberOfNodes)
    distancesWithNodeIndex.withFilter{ case (d, _) => d >= 0 }.map{ case (_, k) => k }.toSet
  }

  def depthFirstSearch(graph: Graph): DepthFirstSearchOutput = {
    val visitStarted: Array[Int] = Array.fill(graph.numberOfNodes)(0)
    val visitEnded: Array[Int] = Array.fill(graph.numberOfNodes)(0)
    val topologicalSorting: MutableStack[Node] = MutableStack()
    val previsitId: Iterator[Int] = Iterator.from(1)
    val postvisitId: Iterator[Int] = Iterator.from(1)

    def isNodeVisisted(node: Node): Boolean = visitStarted(node - 1) > 0
    def findUnvisitedNeighbour(node: Node): Option[Node] =
      graph.adjacencyList.getOrElse(node, Nil).find(neighbour => !isNodeVisisted(neighbour))

    @tailrec
    def loop(nodes: List[Node], components: List[Component]): List[Component] = nodes match {
      case Nil => components
      case node :: remainingNodes =>
        if (isNodeVisisted(node)) loop(remainingNodes, components)
        else {
          val nextComponent: Component = explore(node)
          loop(remainingNodes, nextComponent :: components)
        }
    }

    def explore(startingNode: Node): Component = {
      @tailrec
      def processComponent(previsitStack: List[Node], component: List[Node]): List[Node] = {
        if (previsitStack.isEmpty) component
        else {
          val topNode: Node = previsitStack.head
          findUnvisitedNeighbour(topNode) match {
            case Some(neighbour) =>
              visitStarted(neighbour - 1) = previsitId.next()
              processComponent(neighbour :: previsitStack, neighbour :: component)
            case None =>
              visitEnded(topNode - 1) = postvisitId.next()
              topologicalSorting.push(topNode)
              processComponent(previsitStack.tail, component)
          }
        }
      }

      visitStarted(startingNode - 1) = previsitId.next()
      processComponent(List(startingNode), List(startingNode)).toSet
    }

    val components: List[Component] = loop(graph.nodeOrder, Nil)
    DepthFirstSearchOutput(components, visitStarted, visitEnded, topologicalSorting.toList)
  }

  private def createGraphWithEdgesReversed(graph: Graph, postVisitOrdering: Array[Int]): Graph = {
    val reversedEdges: List[Edge] = graph.edgeList.map(edge => edge.swap)
    val nodeOrder: List[Node] =
      postVisitOrdering.zip(1 to graph.numberOfNodes)
        .sortBy{ case (postNumber, _) => postNumber }(Ordering[Int].reverse)
        .unzip
        ._2
        .toList
    Graph(graph.numberOfNodes, reversedEdges, isDirected = graph.isDirected, Some(nodeOrder))
  }

  def getIncomingEdges(graph: Graph): Map[Node, List[Node]] =
    (for {
      (node, neighbours) <- graph.adjacencyList.toIterator
      neighbour <- neighbours
    } yield (neighbour, node))
      .toList
      .groupBy(_._1)
      .mapValues(edgesFromNode => edgesFromNode.map(_._2))

}
