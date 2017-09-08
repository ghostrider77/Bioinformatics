package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/orf/
  */

object OpenReadingFrames {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_99",
        "AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{Fasta,
  readFastaSequences, readInputData, readRnaCodonTable, writeListOfListsAsStringsToFile}
  import utils.{AminoAcid, Codon, Protein, Dna, Rna, RnaNucleotide}
  import utils.Dna.transcribe

  val inputFileName: String = "/stronghold/datasets/rosalind_orf.txt"

  private val startCodon: Codon = Codon(Rna("AUG").sequence)

  def getData(isPractice: Boolean): Dna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(Fasta(_, sequence)): List[Fasta] = readFastaSequences(data, isLineSeparated = false)
    Dna(sequence)
  }

  def translateAtOpenReadingFrames(dna: Dna, codonTable: Map[Codon, Option[AminoAcid]]): Set[Protein] = {
    val rna: Rna = transcribe(dna)
    val proteinsFromRna: Set[Protein] = translate(rna, codonTable)
    val rnaFromReverseComplement: Rna = transcribe(dna.reverseComplement)
    val proteinsFromReverseComplement: Set[Protein] = translate(rnaFromReverseComplement, codonTable)
    proteinsFromRna ++ proteinsFromReverseComplement
  }

  def getProteinStartingAtIndex(rna: Rna, codonTable: Map[Codon, Option[AminoAcid]]): Option[Protein] = {
    val nucleotideTriplets: Iterator[List[RnaNucleotide]] = rna.sequence.grouped(3).filter(_.length == 3)

    @tailrec
    def loop(aminoAcids: List[AminoAcid]): Option[Protein] = {
      if (!nucleotideTriplets.hasNext) None
      else {
        val triplet: List[RnaNucleotide] = nucleotideTriplets.next()
        codonTable(Codon(triplet)) match {
          case None => Some(Protein(aminoAcids.reverse))
          case Some(nextAminoAcid) => loop(nextAminoAcid :: aminoAcids)
        }
      }
    }

    loop(Nil)
  }

  def translate(rna: Rna, codonTable: Map[Codon, Option[AminoAcid]]): Set[Protein] = {
    val startCodonIndices: Iterator[Int] =
      for {
        (triplet, ix) <- rna.sequence.sliding(3, 1).zipWithIndex
        if Codon(triplet) == startCodon
      } yield ix

    (for {
      ix <- startCodonIndices
      protein <- getProteinStartingAtIndex(Rna(rna.sequence.drop(ix)), codonTable)
    } yield protein).toSet
  }

  def main(args: Array[String]): Unit = {
    val dna: Dna = getData(isPractice = false)
    val codonTable: Map[Codon, Option[AminoAcid]] = readRnaCodonTable()
    val proteins: Set[Protein] = translateAtOpenReadingFrames(dna, codonTable)
    writeListOfListsAsStringsToFile(proteins.toList.map(p => List(p.toString)))
  }

}
