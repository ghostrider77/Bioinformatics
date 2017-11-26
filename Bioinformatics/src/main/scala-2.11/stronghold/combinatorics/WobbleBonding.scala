package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/rnas/
  */

object WobbleBonding {

  object SampleData {
    val sample: List[String] = List("AUGCUAGUACGGAGCGAGUCUAGCGAGCGAUGUCGUGAGUACUAUAUAUGCGCAUAAGCCACGU")
  }

  import scala.collection.mutable.{Map => MutableMap}
  import SampleData.sample
  import utils.UtilityFunctions.readInputData
  import utils.Rna

  val inputFileName: String = "/stronghold/datasets/rosalind_rnas.txt"

  private val validBondings: Map[Char, Set[Char]] =
    Map('A' -> Set('U'), 'C' -> Set('G'), 'G' -> Set('C', 'U'), 'U' -> Set('A', 'G'))

  def getData(isPractice: Boolean): Rna = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    Rna(data.head)
  }

  def isValidMatch(nucleotide1: Char, nucleotide2: Char): Boolean = validBondings(nucleotide1).contains(nucleotide2)

  def calcNumberOfValidMatchings(rna: Rna): BigInt = {
    val numberOfMatchings: MutableMap[String, BigInt] = MutableMap()

    def calcNumberOfMatchings(sequence: String): BigInt = {
      val sequenceLength: Int = sequence.length
      if (sequenceLength == 0 || sequenceLength == 1) 1
      else numberOfMatchings.get(sequence) match {
        case Some(number) => number
        case None =>
          val initialMatchings: BigInt = calcNumberOfMatchings(sequence.drop(1))
          val firstNucleotide: Char = sequence(0)
          val numberOfNoncrossingMatches: BigInt = (4 until sequenceLength).foldLeft(initialMatchings){
            case (acc, ix) =>
              if (isValidMatch(firstNucleotide, sequence(ix)))
                acc + calcNumberOfMatchings(sequence.slice(1, ix)) * calcNumberOfMatchings(sequence.drop(ix + 1))
              else acc
          }
          numberOfMatchings.getOrElseUpdate(sequence, numberOfNoncrossingMatches)
      }
    }

    calcNumberOfMatchings(rna.toString)
  }

  def main(args: Array[String]): Unit = {
    val rna: Rna = getData(isPractice = false)
    val result: BigInt = calcNumberOfValidMatchings(rna)
    println(result)
  }

}
