package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/seto/
  */

object SetOperations {

  object SampleData {
    val sample: List[String] =
      List(
        "10",
        "{1, 2, 3, 4, 5}",
        "{2, 8, 5, 10}"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_seto.txt"

  def getSetElements(s: String): Set[Int] = s.substring(1, s.length-1).split(",").map(_.trim.toInt).toSet

  def getData(isPractice: Boolean): (Int, Set[Int], Set[Int]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val n: Int = data.head.toInt
    val s1: Set[Int] = getSetElements(data(1))
    val s2: Set[Int] = getSetElements(data(2))
    (n, s1, s2)
  }

  def createSets(n: Int, s1: Set[Int], s2: Set[Int]): List[Set[Int]] = {
    val universalSet: Set[Int] = (1 to n).toSet
    List(s1.union(s2), s1.intersect(s2), s1.diff(s2), s2.diff(s1), universalSet.diff(s1), universalSet.diff(s2))
  }

  def transformToPrintableForm(sets: List[Set[Int]]): List[String] = sets.map(_.mkString("{", ", ", "}"))

  def main(args: Array[String]): Unit = {
    val (n, s1, s2): (Int, Set[Int], Set[Int]) = getData(isPractice = false)
    val result: List[Set[Int]] = createSets(n, s1, s2)
    writeListOfListsAsStringsToFile(transformToPrintableForm(result).map(s => List(s)))
  }

}
