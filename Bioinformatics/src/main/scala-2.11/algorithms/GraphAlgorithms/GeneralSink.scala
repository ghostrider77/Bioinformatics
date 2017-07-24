package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/gs/
  */

object GeneralSink {

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

  val inputFileName: String = "/algorithms/datasets/rosalind_gs.txt"

  def getData(isPractice: Boolean): List[Graph] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readListOfGraphs(data, isDirected = true)
  }

  def componentHasIncomingEdge(component: Component, incomingEdges: Map[Node, List[Node]]): Boolean = {
    val nodesWithEdgeIntoComponent: Set[Node] = component.flatMap(node => incomingEdges.getOrElse(node, Nil))
    nodesWithEdgeIntoComponent.diff(component).nonEmpty
  }

  def findSourceInDirectedGraph(graph: Graph): Int = {
    val stronglyConnectedComponents: List[Component] = graph.stronglyConnectedComponents.get.reverse
    val sourceComponent: Component = stronglyConnectedComponents.head
    val incomingEdges: Map[Node, List[Node]] = getIncomingEdges(graph)

    @tailrec
    def loop(components: List[Component]): Node = {
      if (components.isEmpty) sourceComponent.head
      else if (!componentHasIncomingEdge(components.head, incomingEdges)) -1
      else loop(components.tail)
    }

    loop(stronglyConnectedComponents.tail)
  }

  def main(args: Array[String]): Unit = {
    val graphs: List[Graph] = getData(isPractice = false)
    val result: List[Int] = graphs.map(findSourceInDirectedGraph)
    writeListAsStringToFile(result)
  }

}
