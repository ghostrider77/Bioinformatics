package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/prob/
  */

object RandomStrings {

  object SampleData {
    val sample: List[String] =
      List(
        "ACGATACAA",
        "0.129 0.287 0.423 0.476 0.641 0.742 0.783"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}
  import utils.{Dna, DnaNucleotide, Adenine, Cytosine, Guanine, Thymine}

  val inputFileName: String = "/stronghold/datasets/rosalind_prob.txt"

  def getData(isPractice: Boolean): (Dna, List[Double]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val dna: Dna = Dna(data.head)
    val gcContent: List[Double] = data(1).split(" ").map(_.toDouble).toList
    (dna, gcContent)
  }

  def logprobabilityThatRandomStringWithGivenGcContentMatchesDna(dna: Dna, gcContents: List[Double]): List[Double] = {
    val nucleotideCounts: Map[DnaNucleotide, Int] = dna.sequence.groupBy(identity).mapValues(_.length)
    for { gc <- gcContents } yield {
      (nucleotideCounts.getOrElse(Guanine, 0) + nucleotideCounts.getOrElse(Cytosine, 0)) * math.log10(gc / 2) +
        (nucleotideCounts.getOrElse(Adenine, 0) + nucleotideCounts.getOrElse(Thymine, 0)) * math.log10((1 - gc) / 2)
    }
  }

  def main(args: Array[String]): Unit = {
    val (dna, gcContents): (Dna, List[Double]) = getData(isPractice = false)
    val result: List[Double] = logprobabilityThatRandomStringWithGivenGcContentMatchesDna(dna, gcContents)
    writeListAsStringToFile(result)
  }

}
