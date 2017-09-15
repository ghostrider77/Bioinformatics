package stronghold.alignment

/**
  * problem description: http://rosalind.info/problems/tran/
  */

object TransitionsAndTransversions {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_0209",
        "GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGA",
        "AGTACGGGCATCAACCCAGTT",
        ">Rosalind_2200",
        "TTATCTGACAAAGAAAGCCGTCAACGGCTGGATAATTTCGCGATCGTGCTGGTTACTGGC",
        "GGTACGAGTGTTCCTTTGGGT"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readFastaSequences, readInputData}
  import utils.{Dna, DnaNucleotide, Adenine, Cytosine, Guanine, Thymine}

  val inputFileName: String = "/stronghold/datasets/rosalind_tran.txt"

  private val Transitions: Set[(DnaNucleotide, DnaNucleotide)] =
    Set((Adenine, Guanine), (Guanine, Adenine), (Cytosine, Thymine), (Thymine, Cytosine))

  def getData(isPractice: Boolean): List[Dna] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val fastaSequences: List[Fasta] = readFastaSequences(data, isLineSeparated = false)
    fastaSequences.map{ case Fasta(_, sequence) => Dna(sequence) }
  }

  def calcTransitionTransversionRatio(dna1: Dna, dna2: Dna): Double = {
    def countMutations(acc: (Int, Int), nucleotidePair: (DnaNucleotide, DnaNucleotide)): (Int, Int) = {
      val (transitions, transversions): (Int, Int) = acc
      if (nucleotidePair._1 == nucleotidePair._2) acc
      else if (Transitions.contains(nucleotidePair)) (transitions + 1, transversions)
      else (transitions, transversions + 1)
    }

    val (transitions, transversions): (Int, Int) =
      dna1.sequence.view.zip(dna2.sequence).foldLeft((0, 0))(countMutations)
    transitions / transversions.toDouble
  }

  def main(args: Array[String]): Unit = {
    val List(dna1, dna2): List[Dna] = getData(isPractice = false)
    val result: Double = calcTransitionTransversionRatio(dna1, dna2)
    println(result)
  }

}
