package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/mrna/
  */

object InferingMRnaFromProtein {

  object SampleData {
    val sample: List[String] = List("MA")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, readRnaCodonTable}
  import utils.{AminoAcid, Codon, Protein, Rna, RnaNucleotide}

  val inputFileName: String = "/stronghold/datasets/rosalind_mrna.txt"

  val Modulus: Int = 1000000

  def getData(isPractice: Boolean): Protein = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val aminoAcids: List[AminoAcid] = data.head.map(AminoAcid(_)).toList
    Protein(aminoAcids)
  }

  def calcNumberOfRnaStringsTranslateIntoProtein(protein: Protein, codonTable: Map[Codon, Option[AminoAcid]]): Int = {
    val numberOfCodonsEncodingProtein: List[Int] = for { aminoAcidInProtein <- protein.sequence } yield
      codonTable
        .valuesIterator
        .count(optionalAminoAcid => optionalAminoAcid.nonEmpty && optionalAminoAcid.get == aminoAcidInProtein)
    val numberOfStopCodons: Int = codonTable.valuesIterator.count(_.isEmpty)
    numberOfCodonsEncodingProtein.foldLeft(numberOfStopCodons){ case (acc, number) => acc * number % Modulus }
  }

  def main(args: Array[String]): Unit = {
    val protein: Protein = getData(isPractice = false)
    val codonTable: Map[Codon, Option[AminoAcid]] = readRnaCodonTable()
    val result: Int = calcNumberOfRnaStringsTranslateIntoProtein(protein, codonTable)
    println(result)
  }

}
