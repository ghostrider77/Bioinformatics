package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/revc/
  */

object ComplementingDnaStrand {

  object SampleData {
    val sample: List[String] = List("AAAACCCGGT")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeNucleotideListToFileAsString}
  import utils.Dna

  val inputFileName: String = "/stronghold/datasets/rosalind_revc.txt"

  def getData(isPractice: Boolean): Dna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    Dna(data.head)
  }

  def main(args: Array[String]): Unit = {
    val dna: Dna = getData(isPractice = false)
    val result: Dna = dna.reverseComplement
    writeNucleotideListToFileAsString(result.sequence)
  }
}
