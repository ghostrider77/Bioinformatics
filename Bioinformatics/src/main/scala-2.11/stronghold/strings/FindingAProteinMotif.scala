package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/mprt/
  */

object FindingAProteinMotif {
  type ProteinID = String

  object SampleData {
    val sample: List[String] =
      List(
        "A2Z669",
        "B5ZC00",
        "P07204_TRBM_HUMAN",
        "P20840_SAG1_YEAST"
      )
  }

  import scala.util.matching.Regex
  import SampleData.sample
  import utils.Protein
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_mprt.txt"

  private val ServerAddress: String = "http://www.uniprot.org/uniprot/"
  private val Motif = new Regex("""(?=(N[^P](S|T)[^P]))""")

  def getData(isPractice: Boolean): List[String] = if (isPractice) sample else readInputData(inputFileName)

  def getProteinDataFromUniprotDb(uniprotIds: List[ProteinID]): Map[ProteinID, Protein] =
    (for { proteinId <- uniprotIds } yield {
      val url: String = ServerAddress + proteinId + ".fasta"
      val content: List[String] = scala.io.Source.fromURL(url).getLines.toList
      proteinId -> Protein(content.tail.mkString)
    }).toMap

  def findMotifPosition(proteins: Map[ProteinID, Protein]): Map[ProteinID, List[Int]] =
    for {
      (proteinId, protein) <- proteins
      motifPositions: List[Int] = Motif.findAllMatchIn(protein.toString).map(_.start + 1).toList
      if motifPositions.nonEmpty
    } yield proteinId -> motifPositions

  def main(args: Array[String]): Unit = {
    val uniprotIds: List[ProteinID] = getData(isPractice = false)
    val proteins: Map[ProteinID, Protein] = getProteinDataFromUniprotDb(uniprotIds)
    val result: Map[ProteinID, List[Int]] = findMotifPosition(proteins)
    val output: List[List[String]] =
      result.flatMap{ case (id, positions) => List(List(id), positions.map(_.toString)) }.toList
    writeListOfListsAsStringsToFile(output)
  }
}
