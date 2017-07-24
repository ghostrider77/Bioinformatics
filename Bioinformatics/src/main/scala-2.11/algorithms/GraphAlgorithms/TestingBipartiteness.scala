package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/bip/
  */

object TestingBipartiteness {

  sealed trait Color
  case object Red extends Color
  case object Blue extends Color

  object SampleData {
    val sample: List[String] =
      List(
        "2",
        "",
        "3 3",
        "1 2",
        "3 2",
        "3 1",
        "",
        "4 3",
        "1 4",
        "3 1",
        "1 2"
      )
  }

  import scala.annotation.tailrec
  import collection.mutable.{Map => MutableMap, Set => MutableSet}
  import algorithms.Datastructures.Graph
  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}
  import utils.GraphUtilityFunctions.{Node, readListOfGraphs}

  val inputFileName: String = "/algorithms/datasets/rosalind_bip.txt"

  def getData(isPractice: Boolean): List[Graph] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readListOfGraphs(data, isDirected = false)
  }

  def getNeighboursOfNodes(nodes: Set[Node],
                           adjacencyList: Map[Node, List[Node]],
                           coloring: MutableMap[Node, Color]): (Set[Node], Set[Node]) = {
    val unColoredNeighbours: MutableSet[Node] = MutableSet()
    val coloredNeighbours: MutableSet[Node] = MutableSet()
    for {
      node <- nodes
      neighbours: List[Node] = adjacencyList.getOrElse(node, Nil)
      neighbour <- neighbours
    } {
      if (coloring.contains(neighbour)) coloredNeighbours += neighbour
      else unColoredNeighbours += neighbour
    }

    (unColoredNeighbours.toSet, coloredNeighbours.toSet)
  }

  def changeColor(color: Color): Color = color match {
    case Red => Blue
    case Blue => Red
  }

  def updateColoredNodes(uncoloredNodes: Set[Node], color: Color, coloring: MutableMap[Node, Color]): Unit =
    for { node <- uncoloredNodes} coloring += (node -> color)

  def isColoringConsistent(color: Color, coloredNeighbours: Set[Node], coloring: MutableMap[Node, Color]): Boolean =
    coloredNeighbours.forall(coloredNode => coloring(coloredNode) == color)

  def colorComponent(startingNode: Node,
                     adjacencyList: Map[Node, List[Node]],
                     coloring: MutableMap[Node, Color]): Boolean = {
    val currentColor: Color = Red
    coloring += (startingNode -> currentColor)
    val (neighbours, _): (Set[Node], Set[Node]) = getNeighboursOfNodes(Set(startingNode), adjacencyList, coloring)

    @tailrec
    def loop(unColoredNeighbours: Set[Node], previousColor: Color): Boolean = {
      if (unColoredNeighbours.isEmpty) true
      else {
        val currentColor: Color = changeColor(previousColor)
        updateColoredNodes(unColoredNeighbours, currentColor, coloring)
        val (unvisitedNeighbours, coloredNeighbours): (Set[Node], Set[Node]) =
          getNeighboursOfNodes(unColoredNeighbours, adjacencyList, coloring)
        if (!isColoringConsistent(changeColor(currentColor), coloredNeighbours, coloring)) false
        else loop(unvisitedNeighbours, currentColor)
      }
    }

    loop(neighbours, currentColor)
  }

  def isBipartite(graph: Graph): Boolean = {
    val coloring: MutableMap[Node, Color] = MutableMap()

    def findUncoloredNode(): Option[Node] = (1 to graph.numberOfNodes).find(node => !coloring.contains(node))

    @tailrec
    def loop(): Boolean =
      findUncoloredNode() match {
        case None => true
        case Some(startingNode) =>
          val isComponentConsistentlyColored: Boolean = colorComponent(startingNode, graph.adjacencyList, coloring)
          if (!isComponentConsistentlyColored) false
          else loop()
      }

    loop()
  }

  def testForBipartiteness(graphs: List[Graph]): List[Boolean] = graphs.map(isBipartite)

  def main(args: Array[String]): Unit = {
    val graphs: List[Graph] = getData(isPractice = false)
    val result: List[Int] = testForBipartiteness(graphs).map(if (_) 1 else -1)
    writeListAsStringToFile(result)
  }

}
