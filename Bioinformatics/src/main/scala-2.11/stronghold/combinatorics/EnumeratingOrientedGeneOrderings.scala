package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/sign/
  */

object EnumeratingOrientedGeneOrderings {
  type SignedPermutation = List[String]

  object SampleData {
    val sample: List[String] = List("2")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_sign.txt"

  def getData(isPractice: Boolean): Int = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.head.toInt
  }

  def getSignedPermutations(n: Int): List[SignedPermutation] = {
    val permutations: Iterator[List[Int]] = (1 to n).toList.permutations
    val indicesOfPositiveSigns: List[List[Int]] = (0 to n).toList.flatMap(k => (0 until n).toList.combinations(k))
    (for {
      permutation <- permutations
      indices <- indicesOfPositiveSigns
    } yield {
      permutation.zipWithIndex.map{ case (number, ix) =>
        val sign: String = if (indices.contains(ix)) "" else "-"
        sign + number.toString
      }
    }).toList
  }

  def main(args: Array[String]): Unit = {
    val n: Int = getData(isPractice = false)
    val numberOfSignedPermutations: Int = (1 to n).product * math.pow(2, n).toInt
    val result: List[SignedPermutation] = getSignedPermutations(n)
    writeListOfListsAsStringsToFile(List(numberOfSignedPermutations.toString) :: result)
  }

}
