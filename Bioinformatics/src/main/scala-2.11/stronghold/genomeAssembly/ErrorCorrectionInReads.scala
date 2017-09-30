package stronghold.genomeAssembly

/**
  * problem description: http://rosalind.info/problems/corr/
  */

object ErrorCorrectionInReads {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_52",
        "TCATC",
        ">Rosalind_44",
        "TTCAT",
        ">Rosalind_68",
        "TCATC",
        ">Rosalind_28",
        "TGAAA",
        ">Rosalind_95",
        "GAGGA",
        ">Rosalind_66",
        "TTTCA",
        ">Rosalind_33",
        "ATCAA",
        ">Rosalind_21",
        "TTGAT",
        ">Rosalind_18",
        "TTTCC"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readInputData, readFastaSequences, writeListOfListsAsStringsToFile}
  import utils.Dna

  val inputFileName: String = "/stronghold/datasets/rosalind_corr.txt"

  def getData(isPractice: Boolean): List[Dna] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val fastaSequences: List[Fasta] = readFastaSequences(data, isLineSeparated = false)
    fastaSequences.map(sequence => Dna(sequence.string))
  }

  def getReadCounts(reads: List[Dna]): Map[Dna, Int] =
    reads.flatMap{ read =>
      val reverseComplement: Dna = read.reverseComplement
      if (reads.contains(reverseComplement)) List(read, reverseComplement) else List(read)
    }.groupBy(identity).mapValues(_.length)

  def hammingDistance(incorrectRead: Dna, correctRead: Dna): Int =
    incorrectRead.sequence.zip(correctRead.sequence).count { case (n1, n2) => n1 != n2 }

  def findCorrespondingCorrectRead(incorrectRead: Dna, correctReads: List[Dna]): Option[Dna] = {
    @tailrec
    def loop(reads: List[Dna]): Option[Dna] = reads match {
      case Nil => None
      case correctRead :: remainingReads =>
        if (hammingDistance(incorrectRead, correctRead) == 1) Some(correctRead)
        else {
          val reverseComplement: Dna = correctRead.reverseComplement
          if (hammingDistance(incorrectRead, reverseComplement) == 1) Some(reverseComplement)
          else loop(remainingReads)
        }
    }
    loop(correctReads)
  }

  def identifyIncorrectReads(reads: List[Dna]): List[(Dna, Dna)] = {
    val readCounts: Map[Dna, Int] = getReadCounts(reads)
    val correctReads: List[Dna] = (for {
      (string, count) <- readCounts
      if count > 1
    } yield string).toList
    val incorrectReads: List[Dna] = (for {
      (string, count) <- readCounts
      if count == 1
    } yield string).toList
    for { incorrectRead <- incorrectReads } yield {
      val correctRead: Dna = findCorrespondingCorrectRead(incorrectRead, correctReads).get
      (incorrectRead, correctRead)
    }
  }

  def createOutputFormat(result: List[(Dna, Dna)]): List[List[String]] =
    for { (dna1, dna2) <- result } yield List(dna1.toString + "->" + dna2.toString)

  def main(args: Array[String]): Unit = {
    val reads: List[Dna] = getData(isPractice = false)
    val result: List[(Dna, Dna)] = identifyIncorrectReads(reads)
    writeListOfListsAsStringsToFile(createOutputFormat(result))
  }

}
