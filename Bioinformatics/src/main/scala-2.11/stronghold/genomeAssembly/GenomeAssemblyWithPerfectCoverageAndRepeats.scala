package stronghold.genomeAssembly

/**
  * problem description: http://rosalind.info/problems/grep/
  */

object GenomeAssemblyWithPerfectCoverageAndRepeats {

  object SampleData {
    val sample: List[String] =
      List(
        "CAG",
        "AGT",
        "GTT",
        "TTT",
        "TTG",
        "TGG",
        "GGC",
        "GCG",
        "CGT",
        "GTT",
        "TTC",
        "TCA",
        "CAA",
        "AAT",
        "ATT",
        "TTC",
        "TCA"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}
  import DeBruijnGraph.{Path, findAllEulerianPaths}

  val inputFileName: String = "/stronghold/datasets/rosalind_grep.txt"

  def getData(isPractice: Boolean): List[String] = if (isPractice) sample else readInputData(inputFileName)

  def stringSpelledByTheEdges(path: Path): String = {
    val start: String = path.head._1.label
    start + path.map{ case (_, nodeTo) => nodeTo.label.last }.mkString("")
  }

  def getCircularStringSpelledByEdges(path: Path, k: Int): String = stringSpelledByTheEdges(path).dropRight(k - 1)

  def main(args: Array[String]): Unit = {
    val dnaStrings: List[String] = getData(isPractice = false)
    val k: Int = dnaStrings.head.length
    val graph = new DeBruijnGraph(dnaStrings, k)
    val paths: Set[Path] = findAllEulerianPaths(graph.adjacencyList)
    val circularStrings: Set[String] = paths.map(getCircularStringSpelledByEdges(_, k))
    writeListOfListsAsStringsToFile(circularStrings.map(List(_)).toList)
  }

}
