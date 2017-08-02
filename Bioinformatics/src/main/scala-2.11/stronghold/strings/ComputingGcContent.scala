package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/revc/
  */

object ComputingGcContent {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_6404",
        "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC",
        "TCCCACTAATAATTCTGAGG",
        ">Rosalind_5959",
        "CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT",
        "ATATCCATTTGTCAGCAGACACGC",
        ">Rosalind_0808",
        "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC",
        "TGGGAACCTGCGGGCAGTAGGTGGAAT"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readInputData, readFastaSequences}
  import utils.{C, G, Dna}

  val inputFileName: String = "/stronghold/datasets/rosalind_gc.txt"

  def getData(isPractice: Boolean): List[Fasta] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readFastaSequences(data, isLineSeparated = false)
  }

  def calcGcPercentage(sequence: Fasta): Double = {
    val dna: Dna = Dna(sequence.string)
    val gcContent: Int = dna.sequence.count(nucleotide => nucleotide == C || nucleotide == G)
    gcContent.toDouble / dna.length
  }

  def findDnaWithHighestGcPercentage(sequences: List[Fasta]): (Fasta, Double) = {

    @tailrec
    def loop(xs: List[Fasta], acc: (Fasta, Double)): (Fasta, Double) = xs match {
      case Nil => acc
      case sequence :: xss =>
        val currentGcPercentage: Double = calcGcPercentage(sequence)
        if (currentGcPercentage > acc._2) loop(xss, (sequence, currentGcPercentage))
        else loop(xss, acc)
      }

    loop(sequences.tail, (sequences.head, calcGcPercentage(sequences.head)))
  }

  def main(args: Array[String]): Unit = {
    val sequences: List[Fasta] = getData(isPractice = true)
    val (result, gcContent): (Fasta, Double) = findDnaWithHighestGcPercentage(sequences)
    println(result.name)
    println(100 * gcContent)
  }

}
