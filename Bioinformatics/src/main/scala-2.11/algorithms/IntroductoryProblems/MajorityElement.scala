package algorithms.IntroductoryProblems

/**
  * problem description: http://rosalind.info/problems/maj/
  */

object MajorityElement {

  object SampleData {
    val sample: List[String] =
      List(
        "4 8",
        "5 5 5 5 5 5 5 5",
        "8 7 7 7 1 7 3 7",
        "7 1 6 5 10 100 1000 1",
        "5 1 6 7 1 1 10 1"
      )
  }

  import SampleData.sample
  import algorithms.Utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}

  val inputFileName: String = "/algorithms/datasets/rosalind_maj.txt"

  def getData(isPractice: Boolean): (Int, List[List[Int]]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(_, n): List[Int] = convertStringToIntList(data.head)
    val arrays: List[List[Int]] = for { line <- data.tail } yield convertStringToIntList(line)
    (n, arrays)
  }

  def getMajorityElement(array: List[Int], length: Int): Int = {
    val counts: Map[Int, Int] = array.groupBy(identity).mapValues(_.length)
    val majorityElem: Option[(Int, Int)] = counts.find{ case (_, count) => count > length / 2 }
    if (majorityElem.isEmpty) -1 else majorityElem.get._1
  }

  def calcMajorityElements(arrays: List[List[Int]], n: Int): List[Int] =
    for { array <- arrays } yield getMajorityElement(array, n)

  def main(args: Array[String]): Unit = {
    val (n, arrays): (Int, List[List[Int]]) = getData(isPractice = false)
    val result: List[Int] = calcMajorityElements(arrays, n)
    writeListAsStringToFile(result)
  }

}
