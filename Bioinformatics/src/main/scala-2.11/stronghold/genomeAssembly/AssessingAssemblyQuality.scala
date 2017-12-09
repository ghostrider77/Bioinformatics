package stronghold.genomeAssembly

/**
  * problem description: http://rosalind.info/problems/asmq/
  */

object AssessingAssemblyQuality {

  object SampleData {
    val sample: List[String] =
      List(
        "GATTACA",
        "TACTACTAC",
        "ATTGAT",
        "GAAGA"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.readInputData
  import utils.Dna

  val inputFileName: String = "/stronghold/datasets/rosalind_asmq.txt"

  def getData(isPractice: Boolean): List[Dna] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.map(Dna(_))
  }

  def calcNStatistics(dnaStrings: List[Dna], n: Int): Int = {
    val contigLengthCounts: Map[Int, Int] = dnaStrings.map(_.length).groupBy(identity).mapValues(_.length)
    val uniqueLengths: List[Int] = contigLengthCounts.keys.toList.sorted(Ordering[Int].reverse)
    val fraction: Double = contigLengthCounts.foldLeft(0){ case (acc, (l, c)) => acc + l * c} * (n.toDouble / 100)

    @tailrec
    def findLargestL(acc: Int, lengths: List[Int]): Int = {
      if (lengths.isEmpty) uniqueLengths.last
      else {
        val length: Int = lengths.head
        val lengthOfContigsHavingLengthL = acc + length * contigLengthCounts(length)
        if (lengthOfContigsHavingLengthL >= fraction) length
        else findLargestL(lengthOfContigsHavingLengthL, lengths.tail)
      }
    }
    findLargestL(0, uniqueLengths)
  }

  def main(args: Array[String]): Unit = {
    val dnaStrings: List[Dna] = getData(isPractice = false)
    val n50: Int = calcNStatistics(dnaStrings, 50)
    val n75: Int = calcNStatistics(dnaStrings, 75)
    println(n50 + " " + n75)
  }

}
