package algorithms.GraphAlgorithms

/**
  * problem description: http://rosalind.info/problems/dag/
  */

object TestingAcyclicity {

  object SampleData {
    val sample: List[String] =
      List(
        "3",
        "",
        "2 1",
        "1 2",
        "",
        "4 4",
        "4 1",
        "1 2",
        "2 3",
        "3 1",
        "",
        "4 3",
        "4 3",
        "3 2",
        "2 1"
      )
  }

  import algorithms.Datastructures.Graph
  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}
  import utils.GraphUtilityFunctions.readListOfGraphs

  val inputFileName: String = "/algorithms/datasets/rosalind_dag.txt"

  def getData(isPractice: Boolean): List[Graph] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readListOfGraphs(data, isDirected = true)
  }

  def main(args: Array[String]): Unit = {
    val graphs: List[Graph] = getData(isPractice = false)
    val result: List[Int] = graphs.map(graph => graph.isGraphDAG).map(if (_) 1 else -1)
    writeListAsStringToFile(result)
  }

}
