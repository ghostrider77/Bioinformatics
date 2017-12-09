package stronghold.genomeAssembly

/**
  * problem description: http://rosalind.info/problems/dbru/
  */

object ConstructingADeBruijnGraph {

  object SampleData {
    val sample: List[String] =
      List(
        "TGAT",
        "CATG",
        "TCAT",
        "ATGC",
        "CATC",
        "CATC"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}
  import utils.Dna
  import DeBruijnGraph.getEdges

  val inputFileName: String = "/stronghold/datasets/rosalind_dbru.txt"

  def getData(isPractice: Boolean): List[Dna] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.map(Dna(_))
  }

  def extractKMersFromDnaStrings(dnaStrings: List[Dna]): List[String] =
    dnaStrings.flatMap(dna => List(dna.toString, dna.reverseComplement.toString)).distinct

  def createDeBruijnGraph(dnaStrings: List[Dna]): DeBruijnGraph = {
    val kMers: List[String] = extractKMersFromDnaStrings(dnaStrings)
    new DeBruijnGraph(kMers, kMers.head.length)
  }

  def createOutputFormat(graph: DeBruijnGraph): List[List[String]] = {
    val edges: List[(Node, Node)] = getEdges(graph.adjacencyList).toList
    edges.view.map { case (n1, n2) => List(n1.label, n2.label).mkString("(", ", ", ")") }.map(List(_)).toList
  }

  def main(args: Array[String]): Unit = {
    val dnaStrings: List[Dna] = getData(isPractice = false)
    val graph: DeBruijnGraph = createDeBruijnGraph(dnaStrings)
    writeListOfListsAsStringsToFile(createOutputFormat(graph))
  }

}
