package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/deg/
  */

object DegreeArray {

  object SampleData {
    val sample: List[String] =
      List(
        "6 7",
        "1 2",
        "2 3",
        "6 3",
        "5 6",
        "2 5",
        "2 4",
        "4 1"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}
  import utils.GraphUtilityFunctions.{Edge, Node, readEdgeList}

  val inputFileName: String = "/algorithms/datasets/rosalind_deg.txt"

  def getData(isPractice: Boolean): (Int, List[Edge]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val Array(n, _): Array[Int] = data.head.split(" ").map(_.toInt)
    val edges: List[Edge] = readEdgeList(data.tail)
    (n, edges)
  }

  def calcOutDegrees(edgeList: List[Edge]): Map[Node, Int] = {
    val directedEdges: List[Edge] = edgeList.flatMap(edge => List(edge, edge.swap))
    directedEdges.groupBy(_._1).mapValues(_.length)
  }

  def main(args: Array[String]): Unit = {
    val (n, edgeList): (Int, List[Edge]) = getData(isPractice = false)
    val degrees: Map[Node, Int] = calcOutDegrees(edgeList)
    val result: List[Int] = (1 to n).map(k => degrees.getOrElse(k, 0)).toList
    writeListAsStringToFile(result)
  }

}
