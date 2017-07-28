package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/dna/
  */

object CountingDnaNucleotides {

  object SampleData {
    val sample: List[String] = List("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
  }

  import SampleData.sample
  import utils.UtilityFunctions.readInputData
  import utils.DnaNucleotide
  import utils.Dna
  import utils.Dna.convertDNAMapToList

  val inputFileName: String = "/stronghold/datasets/rosalind_dna.txt"

  def getData(isPractice: Boolean): Dna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    Dna(data.head)
  }

  def countNucleotides(dna: Dna): Map[DnaNucleotide, Int] =
    dna.sequence.groupBy(identity).mapValues(_.length)

  def main(args: Array[String]): Unit = {
    val dna: Dna = getData(isPractice = false)
    val result: List[Int] = convertDNAMapToList(countNucleotides(dna))
    println(result.mkString(" "))
  }

}
