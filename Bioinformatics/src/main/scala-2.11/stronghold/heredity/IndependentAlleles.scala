package stronghold.heredity

/**
  * problem description: http://rosalind.info/problems/lia/
  */

object IndependentAlleles {

  object SampleData {
    val sample: List[String] = List("2 1")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}
  import breeze.stats.distributions.Binomial

  val inputFileName: String = "/stronghold/datasets/rosalind_lia.txt"

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data.head)
  }

  def probabilityThatKthGenerationHasAtLeastNAaBb(k: Int, n: Int): Double = {
    val p: Double = 0.25
    val lastGenerationSize: Int = math.pow(2, k).toInt
    val binomialDistribution = Binomial(lastGenerationSize, p)

    (n to lastGenerationSize).foldLeft(0.0)((acc, m) => acc + binomialDistribution.probabilityOf(m))
  }

  def main(args: Array[String]): Unit = {
    val List(k, n): List[Int] = getData(isPractice = false)
    val result: Double = probabilityThatKthGenerationHasAtLeastNAaBb(k, n)
    println(result)
  }

}
