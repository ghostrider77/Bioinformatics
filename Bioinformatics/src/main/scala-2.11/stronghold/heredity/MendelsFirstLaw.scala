package stronghold.heredity

/**
  * problem description: http://rosalind.info/problems/iprb/
  */

object MendelsFirstLaw {

  object SampleData {
    val sample: List[String] = List("2 2 2")
  }

  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}
  import utils.Implicits._

  val inputFileName: String = "/stronghold/datasets/rosalind_iprb.txt"

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data.head)
  }

  def calcProbabilityChildPossessDominantAllele(d: Int, h: Int, r: Int): Double = {
    val dd: Double = 1.0 * d.choose(2)
    val hh: Double = 0.75 * h.choose(2)
    val dh: Double = d * h
    val dr: Double = d * r
    val hr: Double = 0.5 * h * r

    val numberOfPossibleParentPairs: Long = (d + r + h).choose(2)
    (dd + hh + dh + dr + hr) / numberOfPossibleParentPairs
  }

  def main(args: Array[String]): Unit = {
    val List(k, m, n): List[Int] = getData(isPractice = false)
    val result: Double = calcProbabilityChildPossessDominantAllele(k, m, n)
    println(result)
  }

}
