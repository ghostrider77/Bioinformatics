package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/kmer/
  */

object KMerComposition {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_6431",
        "CTTCGAAAGTTTGGGCCGAGTCTTACAGTCGGTCTTGAAGCAAAGTAACGAACTCCACGG",
        "CCCTGACTACCGAACCAGTTGTGAGTACTCAACTGGGTGAGAGTGCAGTCCCTATTGAGT",
        "TTCCGAGACTCACCGGGATTTTCGATCCAGCCTCAGTCCAGTCTTGTGGCCAACTCACCA",
        "AATGACGTTGGAATATCCCTGTCTAGCTCACGCAGTACTTAGTAAGAGGTCGCTGCAGCG",
        "GGGCAAGGAGATCGGAAAATGTGCTCTATATGCGACTAAAGCTCCTAACTTACACGTAGA",
        "CTTGCCCGTGTTAAAAACTCGGCTCACATGCTGTCTGCGGCTGGCTGTATACAGTATCTA",
        "CCTAATACCCTTCAGTTCGCCGCACAAAAGCTGGGAGTTACCGCGGAAATCACAG"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readInputData, readFastaSequences, writeListAsStringToFile}
  import utils.{Dna, DnaNucleotide, Adenine, Cytosine, Guanine, Thymine}

  val inputFileName: String = "/stronghold/datasets/rosalind_kmer.txt"

  type KMer = List[DnaNucleotide]
  private val Alphabet: List[DnaNucleotide] = List(Adenine, Cytosine, Guanine, Thymine)
  private val kValue: Int = 4

  def getData(isPractice: Boolean): Dna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val dnaStrings: List[Fasta] = readFastaSequences(data, isLineSeparated = false)
    Dna(dnaStrings.head.string)
  }

  def getOrderedKmers(alphabet: List[DnaNucleotide], k: Int): List[KMer] =
    (0 until k).foldLeft(List(Nil): List[KMer]) {
      case (acc, _) => alphabet.flatMap(nucleotide => acc.map(kMer => nucleotide :: kMer))
    }

  def countKMersInDna(dna: Dna, k: Int): Map[KMer, Int] =
    dna.sequence.sliding(k).toList.groupBy(identity).mapValues(_.length)

  def createKMerCompositionOfDna(dna: Dna, alphabet: List[DnaNucleotide], k: Int): List[Int] = {
    val kMerCounts: Map[KMer, Int] = countKMersInDna(dna, k)
    val orderedKMers: List[KMer] = getOrderedKmers(alphabet, k)
    for { kMer <- orderedKMers } yield kMerCounts.getOrElse(kMer, 0)
  }

  def main(args: Array[String]): Unit = {
    val dna: Dna = getData(isPractice = false)
    val result: List[Int] = createKMerCompositionOfDna(dna, Alphabet, kValue)
    writeListAsStringToFile(result)
  }

}
