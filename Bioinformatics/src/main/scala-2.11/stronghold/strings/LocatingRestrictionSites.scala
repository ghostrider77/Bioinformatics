package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/revp/
  */

object LocatingRestrictionSites {

  type PositionAndLength = (Int, Int)

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_24",
        "TCAATGCATGCGGGTCTATATGCAT"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readInputData, readFastaSequences, writeListOfListsAsStringsToFile}
  import utils.Dna

  val inputFileName: String = "/stronghold/datasets/rosalind_revp.txt"

  private val minLength: Int = 4
  private val maxLength: Int = 12

  def getData(isPractice: Boolean): List[Fasta] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readFastaSequences(data, isLineSeparated = false)
  }

  def findReversePalindromes(dna: Dna): List[PositionAndLength] = {
    val dnaString: String = dna.toString
    (for {
      substringLength <- minLength to maxLength
      ix <- 0 to dna.length - substringLength
      substring: String = dnaString.substring(ix, ix + substringLength)
      if substring == Dna(substring).reverseComplement.toString
    } yield (ix, substringLength)).toList
  }

  def main(args: Array[String]): Unit = {
    val List(sequence): List[Fasta] = getData(isPractice = false)
    val dna: Dna = Dna(sequence.string)
    val result: List[PositionAndLength] = findReversePalindromes(dna).map{ case (ix, length) => (ix + 1, length) }
    writeListOfListsAsStringsToFile(result.map{ case (p, l) => List(p, l) })
  }
}
