package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/pmch/
  */

object PerfectMatchings {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_23",
        "AGCUAGUCAU"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readFastaSequences, readInputData}
  import utils.{Rna, RnaNucleotide, Adenine, Cytosine}

  val inputFileName: String = "/stronghold/datasets/rosalind_pmch.txt"

  def getData(isPractice: Boolean): Rna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val Fasta(_, sequence): Fasta = readFastaSequences(data, isLineSeparated = false).head
    Rna(sequence)
  }

  def factorial(n: Int): BigInt = {
    @tailrec
    def loop(acc: BigInt, k: Int): BigInt =
      if (k <= 1) acc
      else loop(acc * k, k - 1)

    loop(1, n)
  }

  def calcNumberOfPerfectMatchings(rna: Rna): BigInt = {
    val nucleotideCounts: Map[RnaNucleotide, Int] = rna.sequence.groupBy(identity).mapValues(_.length)
    val countAdenine: Int = nucleotideCounts.getOrElse(Adenine, 0)
    val countCytosine: Int = nucleotideCounts.getOrElse(Cytosine, 0)
    factorial(countAdenine) * factorial(countCytosine)
  }

  def main(args: Array[String]): Unit = {
    val rna: Rna = getData(isPractice = true)
    val result: BigInt = calcNumberOfPerfectMatchings(rna)
    println(result)
  }

}
