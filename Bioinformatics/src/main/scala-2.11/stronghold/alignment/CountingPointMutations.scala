package stronghold.alignment

/**
  * problem description: http://rosalind.info/problems/hamm/
  */

object CountingPointMutations {

  object SampleData {
    val sample: List[String] =
      List(
        "GAGCCTACTAACGGGAT",
        "CATCGTAATGACGGCCT"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.readInputData

  val inputFileName: String = "/stronghold/datasets/rosalind_hamm.txt"

  def getData(isPractice: Boolean): (String, String) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    (data.head, data(1))
  }

  def calcHammingDistance(s1: List[Char], s2: List[Char]): Int = s1.view.zip(s2).count{ case (c1, c2) => c1 != c2 }

  def main(args: Array[String]): Unit = {
    val (s1, s2): (String, String) = getData(isPractice = false)
    val result: Int = calcHammingDistance(s1.toList, s2.toList)
    println(result)
  }

}
