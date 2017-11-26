package stronghold.massSpectrometry

/**
  * problem description: http://rosalind.info/problems/conv/
  */

object SpectralConvolution {

  object SampleData {
    val sample: List[String] =
      List(
        "186.07931 287.12699 548.20532 580.18077 681.22845 706.27446 782.27613 968.35544 968.35544",
        "101.04768 158.06914 202.09536 318.09979 419.14747 463.17369"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.readInputData

  val inputFileName: String = "/stronghold/datasets/rosalind_conv.txt"

  def getData(isPractice: Boolean): List[List[Double]] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.map(_.split(" ").map(_.toDouble).toList)
  }

  def round(number: Double, digits: Int): Double = math.round(number * math.pow(10, digits)) / math.pow(10, digits)

  def calcConvolutionOfSpectra(spectrum1: List[Double], spectrum2: List[Double]): (Int, Double) = {
    val convolution: Map[String, Int] = (for {
      mass1 <- spectrum1
      mass2 <- spectrum2
    } yield round(mass1 - mass2, 4).toString).groupBy(identity).mapValues(_.length)
    val (shift, multiplicity): (String, Int) = convolution.maxBy{ case (_, count) => count }
    (multiplicity, math.abs(shift.toDouble))
  }

  def main(args: Array[String]): Unit = {
    val List(spectrum1, spectrum2): List[List[Double]] = getData(isPractice = false)
    val (multiplicity, shift): (Int, Double) = calcConvolutionOfSpectra(spectrum1, spectrum2)
    println(multiplicity)
    println(shift)
  }

}
