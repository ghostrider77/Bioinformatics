package stronghold.massSpectrometry

/**
  * problem description: http://rosalind.info/problems/spec/
  */

object InferringProteinFromSpectrum {

  object SampleData {
    val sample: List[String] =
      List(
        "3524.8542",
        "3710.9335",
        "3841.974",
        "3970.0326",
        "4057.0646"
      )
  }

  import SampleData.sample
  import utils.{AminoAcid, Protein}
  import utils.UtilityFunctions.{readInputData, readMonoisotopicMassTable}

  val inputFileName: String = "/stronghold/datasets/rosalind_spec.txt"

  private lazy val aminoAcidMassTable: Map[AminoAcid, Double] = readMonoisotopicMassTable()

  private val absoluteTolerance: Double = 1e-02

  def getData(isPractice: Boolean): List[Double] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.map(_.toDouble)
  }

  def findAminoAcidWithGivenWeight(mass: Double): AminoAcid =
    aminoAcidMassTable.find{ case (_, aminoAcidMass) => math.abs(mass - aminoAcidMass) < absoluteTolerance }.get._1

  def identifyProtein(aminoAcidMassesInProtein: List[Double]): Protein =
    Protein(aminoAcidMassesInProtein.map(findAminoAcidWithGivenWeight))

  def getProteinFromPrefixSpectrum(prefixSpectrum: List[Double]): Protein = {
    val aminoAcidMassesInProtein: List[Double] =
      prefixSpectrum.sorted.sliding(2, 1).map{ case List(mass1, mass2) => mass2 - mass1 }.toList
    identifyProtein(aminoAcidMassesInProtein)
  }

  def main(args: Array[String]): Unit = {
    val prefixSpectrum: List[Double] = getData(isPractice = false)
    val result: Protein = getProteinFromPrefixSpectrum(prefixSpectrum)
    println(result)
  }
}
