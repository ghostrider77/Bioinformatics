package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/subs/
  */

object FindingAMotifInDna {

  object SampleData {
    val sample: List[String] =
      List(
        "GATATATGCATATACTT",
        "ATAT"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{convertToOneBasedIndexing, readInputData, writeListAsStringToFile}
  import utils.{Dna, DnaNucleotide}

  val inputFileName: String = "/stronghold/datasets/rosalind_subs.txt"

  def getData(isPractice: Boolean): List[Dna] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.map(Dna(_))
  }

  def findStartingPositionsOfMotif(dna: Dna, motif: Dna): List[Int] = {
    val text: Vector[DnaNucleotide] = dna.sequence.toVector
    val pattern: Vector[DnaNucleotide] = motif.sequence.toVector

    @tailrec
    def loop(startingIndex: Int, matchedIndices: List[Int]): List[Int] = {
      if (startingIndex > dna.length - motif.length) matchedIndices
      else {
        val subText: Vector[DnaNucleotide] = text.slice(startingIndex, startingIndex + motif.length)
        if (subText == pattern) loop(startingIndex + 1, startingIndex :: matchedIndices)
        else loop(startingIndex + 1, matchedIndices)
      }
    }

    loop(0, Nil).reverse
  }

  def main(args: Array[String]): Unit = {
    val List(dna, motif): List[Dna] = getData(isPractice = false)
    val result: List[Int] = findStartingPositionsOfMotif(dna, motif)
    writeListAsStringToFile(convertToOneBasedIndexing(result))
  }

}
