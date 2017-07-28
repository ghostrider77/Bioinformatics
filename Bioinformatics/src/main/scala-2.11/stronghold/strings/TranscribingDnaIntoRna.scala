package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/rna/
  */

object TranscribingDnaIntoRna {

  object SampleData {
    val sample: List[String] = List("GATGGAACTTGACTACGTAAATT")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeNucleotideListToFileAsString}
  import utils.{Dna, Rna}
  import utils.Dna.transcribe

  val inputFileName: String = "/stronghold/datasets/rosalind_rna.txt"

  def getData(isPractice: Boolean): Dna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    Dna(data.head)
  }

  def main(args: Array[String]): Unit = {
    val dna: Dna = getData(isPractice = false)
    val rna: Rna = transcribe(dna)
    writeNucleotideListToFileAsString(rna.sequence)
  }

}
