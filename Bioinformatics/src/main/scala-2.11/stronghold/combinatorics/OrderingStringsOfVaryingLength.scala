package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/lexv/
  */

object OrderingStringsOfVaryingLength {

  object SampleData {
    val sample: List[String] =
      List(
        "D N A",
        "3"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_lexv.txt"

  def getData(isPractice: Boolean): (List[String], Int) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val alphabet: List[String] = data.head.split(" ").toList
    val n: Int = data(1).toInt
    (alphabet, n)
  }

  def generateOrderedAtMostKmers(alphabet: List[String], k: Int): List[String] = {
    def createOrderedStrings(acc: List[String], l: Int, currentWord: String): List[String] = {
      val orderedStrings: List[String] = currentWord :: acc
      if (l == 0) orderedStrings
      else alphabet.foldLeft(orderedStrings){
        case (innerAcc, letter) => createOrderedStrings(innerAcc, l - 1, currentWord + letter)
      }
    }

    createOrderedStrings(Nil, k, "").reverse.tail
  }

  def main(args: Array[String]): Unit = {
    val (alphabet, k): (List[String], Int) = getData(isPractice = false)
    val result: List[String] = generateOrderedAtMostKmers(alphabet, k)
    writeListOfListsAsStringsToFile(result.map(_.toList), sep = "")
  }

}
