package stronghold.heredity

/**
  * problem description: http://rosalind.info/problems/iev/
  */

object CalculatingExpectedOffspring {

  object SampleData {
    val sample: List[String] = List("1 0 0 1 0 1")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}

  val inputFileName: String = "/stronghold/datasets/rosalind_iev.txt"

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data.head)
  }

  def calcNumberOfOffspringsDisplayingDominantGenotype(numberOfGenotypePairings: List[Int]): Double = {
    val numberOfOffspings: Int = 2
    val probOffspringDisplayDominantPhenotype: List[Double] = List(1.0, 1.0, 1.0, 0.75, 0.5, 0.0)
    numberOfGenotypePairings.zip(probOffspringDisplayDominantPhenotype)
      .foldLeft(0.0){ case (acc, (n, p)) => acc + n * p } * numberOfOffspings
  }

  def main(args: Array[String]): Unit = {
    val numberOfGenotypePairingCouples: List[Int] = getData(isPractice = false)
    val result: Double = calcNumberOfOffspringsDisplayingDominantGenotype(numberOfGenotypePairingCouples)
    println(result)
  }

}
