package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/splc/
  */

object RnaSplicing {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_10",
        "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG",
        ">Rosalind_12",
        "ATCGGTCGAA",
        ">Rosalind_15",
        "ATCGGTCGAGCGTGT"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{Fasta,
    readFastaSequences, readInputData, readRnaCodonTable, writeProteinToFileAsString}
  import utils.{AminoAcid, Codon, Protein, Dna, Rna, RnaNucleotide}
  import utils.Dna.transcribe

  val inputFileName: String = "/stronghold/datasets/rosalind_splc.txt"

  private val startCodon: Codon = Codon(Rna("AUG").sequence)

  def getData(isPractice: Boolean): (Dna, Set[Dna]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val fastaSequences: List[Fasta] = readFastaSequences(data, isLineSeparated = false)
    (Dna(fastaSequences.head.string), fastaSequences.tail.map(s => Dna(s.string)).toSet)
  }

  def translate(rna: Rna, codonTable: Map[Codon, Option[AminoAcid]]): Option[Protein] = {
    val startCodonIndex: Int = rna.sequence.sliding(3, 1).indexWhere(triplet => Codon(triplet) == startCodon)
    if (startCodonIndex == -1) None
    else {
      val nucleotideTriplets: Iterator[List[RnaNucleotide]] =
        rna.sequence.drop(startCodonIndex).grouped(3).filter(_.length == 3)

      @tailrec
      def loop(protein: List[AminoAcid]): List[AminoAcid] = {
        if (!nucleotideTriplets.hasNext) protein
        else {
          val triplet: List[RnaNucleotide] = nucleotideTriplets.next()
          codonTable(Codon(triplet)) match {
            case Some(aminoAcid) => loop(aminoAcid :: protein)
            case None => protein
          }
        }
      }

      Some(Protein(loop(Nil).reverse))
    }
  }

  def findAndTranslateExons(dna: Dna, introns: Set[Dna], codonTable: Map[Codon, Option[AminoAcid]]): Option[Protein] = {
    val exons: Dna =
      Dna(introns.foldLeft(dna.toString){ case (dnaSeq, intron) => dnaSeq.replaceAll(intron.toString, "") })
    translate(transcribe(exons), codonTable)
  }

  def main(args: Array[String]): Unit = {
    val (dna, introns): (Dna, Set[Dna]) = getData(isPractice = false)
    val codonTable: Map[Codon, Option[AminoAcid]] = readRnaCodonTable()
    val protein: Option[Protein] = findAndTranslateExons(dna, introns, codonTable)
    writeProteinToFileAsString(protein.get)
  }

}
