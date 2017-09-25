package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/rstr/
  */

object MatchingRandomMotifs {

  object SampleData {
    val sample: List[String] =
      List(
        "90000 0.6",
        "ATAGCCGA"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.readInputData
  import utils.{Dna, DnaNucleotide, Adenine, Cytosine, Guanine, Thymine}

  val inputFileName: String = "/stronghold/datasets/rosalind_rstr.txt"

  def getData(isPractice: Boolean): (Dna, Int, Double) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val List(n, gcContent): List[Double] = data.head.split(" ").map(_.toDouble).toList
    val dna: Dna = Dna(data(1))
    (dna, n.toInt, gcContent)
  }

  def probabilityThatARandomStringMatchesDna(nucleotideCounts: Map[DnaNucleotide, Int], gcContent: Double): Double = {
    val cgCounts: Int = nucleotideCounts.getOrElse(Guanine, 0) + nucleotideCounts.getOrElse(Cytosine, 0)
    val atCounts: Int = nucleotideCounts.getOrElse(Adenine, 0) + nucleotideCounts.getOrElse(Thymine, 0)
    math.pow(gcContent / 2, cgCounts) * math.pow((1 - gcContent) / 2, atCounts)
  }

  def probabilityThatAtLeastOneRandomStringMatchesDna(dna: Dna, n: Int, gcContent: Double): Double = {
    val nucleotideCounts: Map[DnaNucleotide, Int] = dna.sequence.groupBy(identity).mapValues(_.length)
    val p: Double = probabilityThatARandomStringMatchesDna(nucleotideCounts, gcContent)
    1 - math.pow(1 - p, n)
  }

  def main(args: Array[String]): Unit = {
    val (dna, n, gcContents): (Dna, Int, Double) = getData(isPractice = false)
    val result: Double = probabilityThatAtLeastOneRandomStringMatchesDna(dna, n, gcContents)
    println(result)
  }

}
