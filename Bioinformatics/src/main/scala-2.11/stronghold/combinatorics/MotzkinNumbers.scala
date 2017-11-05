package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/motz/
  */

object MotzkinNumbers {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_57",
        "AUAU"
      )
  }

  import scala.collection.mutable.{Map => MutableMap}
  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readFastaSequences, readInputData}
  import utils.Rna

  val inputFileName: String = "/stronghold/datasets/rosalind_motz.txt"

  private val Modulus: Long = 1000000L

  def getData(isPractice: Boolean): Rna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val Fasta(_, sequence): Fasta = readFastaSequences(data, isLineSeparated = false).head
    Rna(sequence)
  }

  def isMatchingPair(nucleotide1: Char, nucleotide2: Char): Boolean = {
    (nucleotide1 == 'U' && nucleotide2 == 'A') || (nucleotide1 == 'A' && nucleotide2 == 'U') ||
      (nucleotide1 == 'C' && nucleotide2 == 'G') || (nucleotide1 == 'G' && nucleotide2 == 'C')
  }

  def calcNumberOfAllNonCrossingMatchings(rna: Rna): Long = {
    val noncrossingMatchings: MutableMap[String, Long] = MutableMap()

    def calcNumberOfMatchings(sequence: String): Long = {
      val sequenceLength: Int = sequence.length
      if (sequenceLength == 0 || sequenceLength == 1) 1L
      else noncrossingMatchings.get(sequence) match {
        case Some(number) => number
        case None =>
          val initialMatchings: Long = calcNumberOfMatchings(sequence.drop(1))
          val firstNucleotide: Char = sequence(0)
          val numberOfNoncrossingMatches: Long = (1 until sequenceLength).foldLeft(initialMatchings){ case (acc, ix) =>
            if (isMatchingPair(firstNucleotide, sequence(ix)))
              (acc + calcNumberOfMatchings(sequence.slice(1, ix)) * calcNumberOfMatchings(sequence.drop(ix + 1))) %
                Modulus
            else acc
          } % Modulus
          noncrossingMatchings.getOrElseUpdate(sequence, numberOfNoncrossingMatches)
      }
    }

    calcNumberOfMatchings(rna.toString)
  }

  def main(args: Array[String]): Unit = {
    val rna: Rna = getData(isPractice = false)
    val result: Long = calcNumberOfAllNonCrossingMatchings(rna)
    println(result)
  }

}
