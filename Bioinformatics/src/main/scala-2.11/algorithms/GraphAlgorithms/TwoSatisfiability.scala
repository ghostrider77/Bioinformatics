package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/2sat/
  */

object TwoSatisfiability {

  object SampleData {
    val sample: List[String] =
      List(
        "2",
        "",
        "2 4",
        "1 2",
        "-1 2",
        "1 -2",
        "-1 -2",
        "",
        "3 4",
        "1 2",
        "2 3",
        "-1 -2",
        "-2 -3"
      )
  }

  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}
  import utils.GraphUtilityFunctions.{Edge, Node, Component}
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListOfListsAsStringsToFile}
  import SampleData.sample
  import algorithms.Datastructures.Graph

  val inputFileName: String = "/algorithms/datasets/rosalind_2sat.txt"

  def createCNFGraphsFromLogicalFormulae(lines: List[String]): List[Graph] = {
    val reader: Iterator[String] = lines.toIterator
    val numberOfTestCases: Int = reader.next().toInt

    def convertNegativeNode(numberOfVariables: Int, node: Node): Node =
      if (node > 0) node else math.abs(node) + numberOfVariables

    (for { _ <- 0 until numberOfTestCases } yield {
      reader.next()
      val List(numberOfVariables, numberOfClauses): List[Int] = convertStringToIntList(reader.next())
      val edges: List[Edge] =
        (0 until numberOfClauses).flatMap{ _ =>
          val List(literal1, literal2): List[Int] = convertStringToIntList(reader.next())
          List(
            (convertNegativeNode(numberOfVariables, -literal1), convertNegativeNode(numberOfVariables, literal2)),
            (convertNegativeNode(numberOfVariables, -literal2), convertNegativeNode(numberOfVariables, literal1))
          )
        }.toList
      Graph(numberOfNodes = 2 * numberOfVariables, edgeList = edges, isDirected = true)
    }).toList
  }

  def getData(isPractice: Boolean): List[Graph] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    createCNFGraphsFromLogicalFormulae(data)
  }

  def assignBooleanValuesToVariables(graph: Graph, numberOfVariables: Int): Option[Array[Boolean]] = {
    val stronglyConnectedComponents: List[Component] = graph.stronglyConnectedComponents.get.reverse
    val logicalValues: Array[Boolean] = Array.fill(numberOfVariables)(false)

    @tailrec
    def isValidAssignmentPossible(sccs: List[Component]): Boolean = {
      if (sccs.isEmpty) true
      else {
        val (isContradictory, logicalAssignmentInComponent): (Boolean, MutableMap[Node, Boolean]) =
          isComponentContradictory(sccs.head, numberOfVariables)
        if (isContradictory) false
        else {
          for { (node, logicalValue) <- logicalAssignmentInComponent } logicalValues(node - 1) = logicalValue
          isValidAssignmentPossible(sccs.tail)
        }
      }
    }

    if (isValidAssignmentPossible(stronglyConnectedComponents)) Some(logicalValues) else None
  }

  def isComponentContradictory(component: Component, numberOfVariables: Int): (Boolean, MutableMap[Node, Boolean]) = {
    val logicalValuesInComponent: MutableMap[Node, Boolean] = MutableMap()

    @tailrec
    def loop(nodes: Set[Node]): Boolean = {
      if (nodes.isEmpty) false
      else {
        val node: Node = nodes.head
        val (literal, logicalValue): (Node, Boolean) =
          if (node > numberOfVariables) (node - numberOfVariables, false) else (node, true)
        if (logicalValuesInComponent.contains(literal)) true
        else {
          logicalValuesInComponent(literal) = logicalValue
          loop(nodes.tail)
        }
      }
    }

    (loop(component), logicalValuesInComponent)
  }

  def isCNFSatisfiable(graph: Graph): List[Int] = {
    val numberOfLogicalVariables: Int = graph.numberOfNodes / 2
    val logicalAssignment: List[Int] = assignBooleanValuesToVariables(graph, numberOfLogicalVariables) match {
      case None => Nil
      case Some(assignment) =>
        assignment
          .zip(1 to numberOfLogicalVariables)
          .map{ case (logicalValue, literal) => if (logicalValue) literal else -literal }.toList
    }

    if (logicalAssignment.isEmpty) List(0) else 1 :: logicalAssignment
  }

  def main(args: Array[String]): Unit = {
    val cnfGraphs: List[Graph] = getData(isPractice = false)
    val result: List[List[Int]] = cnfGraphs.map(isCNFSatisfiable)
    writeListOfListsAsStringsToFile(result)
  }

}
