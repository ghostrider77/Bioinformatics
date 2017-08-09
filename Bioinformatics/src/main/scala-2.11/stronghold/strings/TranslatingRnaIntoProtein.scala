package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/prot/
  */

object TranslatingRnaIntoProtein {

  object SampleData {
    val sample: List[String] = List("AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, readRnaCodonTable, writeProteinToFileAsString}
  import utils.{AminoAcid, Codon, Protein, Rna, RnaNucleotide}

  val inputFileName: String = "/stronghold/datasets/rosalind_prot.txt"

  def getData(isPractice: Boolean): Rna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    Rna(data.head)
  }

  def translate(rna: Rna, codonTable: Map[Codon, Option[AminoAcid]]): Protein = {
    val nucleotideTriplets: Iterator[List[RnaNucleotide]] = rna.sequence.sliding(3, 3)

    @tailrec
    def loop(protein: List[AminoAcid]): List[AminoAcid] = {
      if (!nucleotideTriplets.hasNext) protein
      else {
        val triplet: List[RnaNucleotide] = nucleotideTriplets.next()
        if (triplet.length != 3) protein
        else {
          codonTable(Codon(triplet)) match {
            case Some(aminoAcid) => loop(aminoAcid :: protein)
            case None => protein
          }
        }
      }
    }

    Protein(loop(Nil).reverse)
  }

  def main(args: Array[String]): Unit = {
    val rna: Rna = getData(isPractice = false)
    val codonTable: Map[Codon, Option[AminoAcid]] = readRnaCodonTable()
    val protein: Protein = translate(rna, codonTable)
    writeProteinToFileAsString(protein)
  }

}