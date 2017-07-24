package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/ddeg/
  */

object DoubleDegreeArray {

  object SampleData {
    val sample: List[String] =
      List(
        "5 4",
        "1 2",
        "2 3",
        "4 3",
        "2 4"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}
  import utils.GraphUtilityFunctions.{Edge, Node, readEdgeList}

  val inputFileName: String = "/algorithms/datasets/rosalind_ddeg.txt"

  def getData(isPractice: Boolean): (Int, List[Edge]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val Array(n, _): Array[Int] = data.head.split(" ").map(_.toInt)
    val edges: List[Edge] = readEdgeList(data.tail)
    (n, edges)
  }

  def calcAdjacencyList(edgelist: List[Edge]): Map[Node, List[Node]] =
    edgelist.groupBy(_._1).mapValues(edgesFromNode => edgesFromNode.map(_._2))

  def calcTotalOutDegreesOfNeighbours(edgeList: List[Edge], n: Int): List[Int] = {
    val directedEdges: List[Edge] = edgeList.flatMap(edge => List(edge, edge.swap))
    val adjacencyList: Map[Node, List[Node]] = calcAdjacencyList(directedEdges)

    (for { node <- 1 to n } yield {
      val neighbours: List[Node] = adjacencyList.getOrElse(node, Nil)
      neighbours.foldLeft(0)((acc, neighbour) => acc + adjacencyList.getOrElse(neighbour, Nil).length)
    }).toList
  }

  def main(args: Array[String]): Unit = {
    val (n, edgeList): (Int, List[Edge]) = getData(isPractice = false)
    val result: List[Int] = calcTotalOutDegreesOfNeighbours(edgeList, n)
    writeListAsStringToFile(result)
  }

}
