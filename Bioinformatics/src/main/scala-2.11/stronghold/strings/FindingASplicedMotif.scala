package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/sseq/
  */

object FindingASplicedMotif {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_14",
        "ACGTACGTGACG",
        ">Rosalind_18",
        "GTA"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, convertToOneBasedIndexing, readInputData, readFastaSequences,
    writeListAsStringToFile}
  import utils.{Dna, DnaNucleotide}

  val inputFileName: String = "/stronghold/datasets/rosalind_sseq.txt"

  def getData(isPractice: Boolean): List[Dna] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val sequences: List[Fasta] = readFastaSequences(data, isLineSeparated = false)
    sequences.map{ case Fasta(_, string) => Dna(string) }
  }

  def findIndicesOfMotifNucleotidesInDna(dna: Dna, motif: Dna): Option[List[Int]] = {
    @tailrec
    def loop(sequenceWithIndex: List[(DnaNucleotide, Int)],
             pattern: List[DnaNucleotide],
             indices: List[Int]): Option[List[Int]] = pattern match {
      case Nil => Some(indices)
      case nucleotideInPattern :: restOfPattern =>
        sequenceWithIndex.find{ case (nucleotideInDna, _) => nucleotideInDna == nucleotideInPattern }.map(_._2) match {
          case None => None
          case Some(ix) =>
            loop(sequenceWithIndex.dropWhile{ case (_, index) => index <= ix }, restOfPattern, ix :: indices)
        }
    }

    loop(dna.sequence.zipWithIndex, motif.sequence, Nil).map(_.reverse)
  }

  def main(args: Array[String]): Unit = {
    val List(dna, motif): List[Dna] = getData(isPractice = true)
    val result: Option[List[Int]] = findIndicesOfMotifNucleotidesInDna(dna, motif)
    println(result.get)
    writeListAsStringToFile(convertToOneBasedIndexing(result.get))
  }

}
