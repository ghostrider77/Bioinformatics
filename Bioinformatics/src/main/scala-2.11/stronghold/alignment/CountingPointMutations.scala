package stronghold.alignment


object CountingPointMutations {

  /**
    * problem description: http://rosalind.info/problems/hamm/
    */

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

  def getData(isPractice: Boolean): (List[Char], List[Char]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    (data.head.toList, data(1).toList)
  }

  def calcHammingDistance(s1: List[Char], s2: List[Char]): Int = s1.view.zip(s2).count{ case (c1, c2) => c1 != c2 }

  def main(args: Array[String]): Unit = {
    val (s1, s2): (List[Char], List[Char]) = getData(isPractice = false)
    val result: Int = calcHammingDistance(s1, s2)
    println(result)
  }

}
