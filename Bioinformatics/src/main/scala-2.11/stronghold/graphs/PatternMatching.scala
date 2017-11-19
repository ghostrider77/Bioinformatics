package stronghold.graphs

/**
  * problem description: http://rosalind.info/problems/trie/
  */

object PatternMatching {

  object SampleData {
    val sample: List[String] =
      List(
        "ATAGA",
        "ATC",
        "GAT"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}
  import Trie.traverseTrie

  val inputFileName: String = "/stronghold/datasets/rosalind_trie.txt"

  def getData(isPractice: Boolean): List[String] = if (isPractice) sample else readInputData(inputFileName)

  def createTrieAdjacencyList(trie: Trie): List[String] = {
    val edges: List[TrieEdge] = traverseTrie(trie).toList
    for { TrieEdge(parentId, childId, letter) <- edges } yield
      List(parentId.toString, childId.toString, letter.toString).mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val strings: List[String] = getData(isPractice = false)
    val trie: Trie = Trie(strings)
    writeListOfListsAsStringsToFile(createTrieAdjacencyList(trie).map(row => List(row)))
  }

}
