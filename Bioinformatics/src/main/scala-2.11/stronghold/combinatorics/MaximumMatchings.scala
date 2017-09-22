package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/mmch/
  */

object MaximumMatchings {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_92",
        "AUGCUUC"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readFastaSequences, readInputData}
  import utils.{Rna, RnaNucleotide, Adenine, Cytosine, Guanine, Uracil}

  val inputFileName: String = "/stronghold/datasets/rosalind_mmch.txt"

  def getData(isPractice: Boolean): Rna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val Fasta(_, sequence): Fasta = readFastaSequences(data, isLineSeparated = false).head
    Rna(sequence)
  }

  def getMaximalMatchesOfNucleotidePairs(nucleotideCounts: Map[RnaNucleotide, Int],
                                         nucleotide1: RnaNucleotide,
                                         nucleotide2: RnaNucleotide): BigInt = {
    val count1: Int = nucleotideCounts(nucleotide1)
    val count2: Int = nucleotideCounts(nucleotide2)
    val countMax: Int = count1 max count2
    val countMin: Int = count1 min count2
    (countMax until (countMax - countMin) by -1).foldLeft(1: BigInt)((acc, elem) => acc * elem)
  }

  def calcNumberOfMaximumMatchings(rna: Rna): BigInt = {
    val nucleotideCounts: Map[RnaNucleotide, Int] = rna.sequence.groupBy(identity).mapValues(_.length)
    val auPairs: BigInt = getMaximalMatchesOfNucleotidePairs(nucleotideCounts, Adenine, Uracil)
    val cgPairs: BigInt = getMaximalMatchesOfNucleotidePairs(nucleotideCounts, Cytosine, Guanine)
    auPairs * cgPairs
  }

  def main(args: Array[String]): Unit = {
    val rna: Rna = getData(isPractice = false)
    val result: BigInt = calcNumberOfMaximumMatchings(rna)
    println(result)
  }

}
