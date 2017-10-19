package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/eval/
  */

object ExpectedNumberOfRestrictionSites {

  object SampleData {
    val sample: List[String] =
      List(
        "10",
        "AG",
        "0.25 0.5 0.75"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListAsStringToFile}
  import utils.{Dna, Adenine, Thymine}

  val inputFileName: String = "/stronghold/datasets/rosalind_eval.txt"

  def getData(isPractice: Boolean): (Int, Dna, List[Double]) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val n: Int = data.head.toInt
    val dna: Dna = Dna(data(1))
    val gcContents: List[Double] = data(2).split(" ").map(_.toDouble).toList
    (n, dna, gcContents)
  }

  def getSubstringProbability(dna: Dna, gcContent: Double): Double =
    dna.sequence.foldLeft(1.0)((acc, nucleotide) =>
      acc * (if (nucleotide == Adenine || nucleotide == Thymine) (1 - gcContent) / 2 else gcContent / 2 ))

  def expectedNumberOfTimesThatStringAppearsInDna(n: Int, dna: Dna, gcContents: List[Double]): List[Double] =
    for { gc <- gcContents } yield {
      val p: Double = getSubstringProbability(dna, gc)
      (n - dna.length + 1) * p
    }

  def main(args: Array[String]): Unit = {
    val (n, dna, gcContents): (Int, Dna, List[Double]) = getData(isPractice = false)
    val result: List[Double] = expectedNumberOfTimesThatStringAppearsInDna(n, dna, gcContents)
    writeListAsStringToFile(result)
  }

}
