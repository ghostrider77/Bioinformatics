package stronghold.phylogeny

/**
  * problem description: http://rosalind.info/problems/pdst/
  */

object CreatingADistanceMatrix {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_9499",
        "TTTCCATTTA",
        ">Rosalind_0942",
        "GATTCATTTC",
        ">Rosalind_6568",
        "TTTCCATTTT",
        ">Rosalind_1833",
        "GTTCCATTTA"
      )
  }

  import SampleData.sample
  import utils.Dna
  import utils.UtilityFunctions.{Fasta, readFastaSequences, readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_pdst.txt"

  def getData(isPractice: Boolean): List[Dna] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val dnaSequences: List[Fasta] = readFastaSequences(data, isLineSeparated = false)
    for { Fasta(_, sequence) <- dnaSequences } yield Dna(sequence)
  }

  def calcDistance(dna1: Dna, dna2: Dna, commonLength: Int): Double =
    dna1.sequence.zip(dna2.sequence).count{ case (n1, n2) => n1 != n2 } / commonLength.toDouble

  def calcDistanceMatrix(dnas: List[Dna]): Array[Array[Double]] = {
    val numberOfSequences: Int = dnas.length
    val commonLength: Int = dnas.head.length
    val distanceMatrix: Array[Array[Double]] = Array.fill(numberOfSequences, numberOfSequences)(0.0)

    for {
      (dna1, ix) <- dnas.zipWithIndex
      (dna2, jy) <- dnas.take(ix).zipWithIndex
    } {
      val distance: Double = calcDistance(dna1, dna2, commonLength)
      distanceMatrix(ix)(jy) = distance
      distanceMatrix(jy)(ix) = distance
    }
    distanceMatrix
  }

  def main(args: Array[String]): Unit = {
    val dnas: List[Dna] = getData(isPractice = false)
    val result: Array[Array[Double]] = calcDistanceMatrix(dnas)
    writeListOfListsAsStringsToFile(result.map(_.toList).toList)
  }
}
