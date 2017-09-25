package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/kmp/
  */

object SpeedingUpMotifFinding {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_87",
        "CAGCATGGTATCACAGCAGAG"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.Dna
  import utils.UtilityFunctions.{readInputData, readFastaSequences, writeListAsStringToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_kmp.txt"

  def getData(isPractice: Boolean): Dna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val sequence: String = readFastaSequences(data, isLineSeparated = false).head.string
    Dna(sequence)
  }

  def createFailureArray(pattern: Vector[Char], patternLength: Int): List[Int] = {
    val failureArray: Array[Int] = Array.fill[Int](patternLength)(0)

    @tailrec
    def loop(positionInPattern: Int, suffixMatchingIndex: Int): Unit = {
      if (positionInPattern < patternLength) {
        if (pattern(positionInPattern) == pattern(suffixMatchingIndex)) {
          failureArray(positionInPattern) = suffixMatchingIndex + 1
          loop(positionInPattern + 1, suffixMatchingIndex + 1)
        }
        else if (suffixMatchingIndex > 0) loop(positionInPattern, failureArray(suffixMatchingIndex - 1))
        else {
          failureArray(positionInPattern) = 0
          loop(positionInPattern + 1, suffixMatchingIndex)
        }
      }
    }

    loop(positionInPattern = 1, suffixMatchingIndex = 0)
    failureArray.toList
  }

  def main(args: Array[String]): Unit = {
    val dna: Dna = getData(isPractice = false)
    val result: List[Int] = createFailureArray(dna.toString.toVector, dna.length)
    writeListAsStringToFile(result, sep = "")
  }

}
