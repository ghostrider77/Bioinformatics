package stronghold.massSpectrometry

/**
  * problem description: http://rosalind.info/problems/prtm/
  */

object CalculatingProteinMass {

  object SampleData {
    val sample: List[String] = List("SKADYEK")
  }

  import SampleData.sample
  import utils.{AminoAcid, Protein}
  import utils.UtilityFunctions.{readInputData, readMonoisotopicMassTable}

  val inputFileName: String = "/stronghold/datasets/rosalind_prtm.txt"

  def getData(isPractice: Boolean): Protein = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    Protein(data.head)
  }

  def calcProteinWeight(protein: Protein, aminoAcidMassTable: Map[AminoAcid, Double]): Double =
    protein.sequence.foldLeft(0.0){ case (acc, aminoAcid) => acc + aminoAcidMassTable(aminoAcid) }

  def main(args: Array[String]): Unit = {
    val protein: Protein = getData(isPractice = false)
    val aminoAcidMassTable: Map[AminoAcid, Double] = readMonoisotopicMassTable()
    val result: Double = calcProteinWeight(protein, aminoAcidMassTable)
    println(result)
  }
}
