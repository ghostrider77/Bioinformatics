package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/lexf/
  */

object EnumeratingKmersLexicographically {

  object SampleData {
    val sample: List[String] =
      List(
        "A C G T",
        "2"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_lexf.txt"

  def getData(isPractice: Boolean): (List[String], Int) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val alphabet: List[String] = data.head.split(" ").toList
    val n: Int = data(1).toInt
    (alphabet, n)
  }

  def getOrderedKmers(alphabet: List[String], k: Int): List[String] =
    (0 until k).foldLeft(List("")){ case (acc, _) => acc.flatMap(nMer => alphabet.map(nMer + _)) }

  def main(args: Array[String]): Unit = {
    val (alphabet, k): (List[String], Int) = getData(isPractice = false)
    val result: List[String] = getOrderedKmers(alphabet, k)
    writeListOfListsAsStringsToFile(result.map(_.toList), sep = "")
  }

}
