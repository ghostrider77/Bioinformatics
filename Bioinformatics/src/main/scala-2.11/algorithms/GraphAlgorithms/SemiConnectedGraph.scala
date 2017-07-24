package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/sc/
  */

object SemiConnectedGraph {

  object SampleData {
    val sample: List[String] =
      List(
        "2",
        "",
        "3 2",
        "3 2",
        "2 1",
        "",
        "3 2",
        "3 2",
        "1 2"
      )
  }

  import scala.annotation.tailrec
  import algorithms.Datastructures.Graph
  import algorithms.Datastructures.Graph.getIncomingEdges
  import SampleData.sample
  import utils.UtilityFunctions.{writeListAsStringToFile, readInputData}
  import utils.GraphUtilityFunctions.{Component, Node, readListOfGraphs}

  val inputFileName: String = "/algorithms/datasets/rosalind_sc.txt"

  def getData(isPractice: Boolean): List[Graph] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readListOfGraphs(data, isDirected = true)
  }

  def noEdgeFromFirstIntoSecond(firstComponent: Component,
                                secondComponent: Component,
                                incomingEdges: Map[Node, List[Node]]): Boolean = {
    val nodesWithEdgesIntoSecond: Set[Node] = secondComponent.flatMap(node => incomingEdges.getOrElse(node, Nil))
    firstComponent.intersect(nodesWithEdgesIntoSecond).isEmpty
  }

  def isGraphSemiConnected(graph: Graph): Boolean = {
    val stronglyConnectedComponents: List[Component] = graph.stronglyConnectedComponents.get.reverse
    val incomingEdges: Map[Node, List[Node]] = getIncomingEdges(graph)

    @tailrec
    def loop(consecutiveComponents: List[(Component, Component)]): Boolean = {
      if (consecutiveComponents.isEmpty) true
      else {
        val (firstComponent, secondComponent): (Component, Component) = consecutiveComponents.head
        if (noEdgeFromFirstIntoSecond(firstComponent, secondComponent, incomingEdges)) false
        else loop(consecutiveComponents.tail)
      }
    }

    loop(stronglyConnectedComponents.zip(stronglyConnectedComponents.tail))
  }

  def main(args: Array[String]): Unit = {
    val graphs: List[Graph] = getData(isPractice = false)
    val result: List[Int] = graphs.map(isGraphSemiConnected).map(if (_) 1 else -1)
    writeListAsStringToFile(result)
  }

}
