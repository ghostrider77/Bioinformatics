package stronghold.strings

/**
  * problem description: http://rosalind.info/problems/cons/
  */

object ConsensusAndProfile {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_1",
        "ATCCAGCT",
        ">Rosalind_2",
        "GGGCAACT",
        ">Rosalind_3",
        "ATGGATCT",
        ">Rosalind_4",
        "AAGCAACC",
        ">Rosalind_5",
        "TTGGAACT",
        ">Rosalind_6",
        "ATGCCATT",
        ">Rosalind_7",
        "ATGGCACT"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readInputData, readFastaSequences, writeListOfListsAsStringsToFile}
  import utils.{Dna, DnaNucleotide}
  import utils.Dna.convertDNAMapToList

  val inputFileName: String = "/stronghold/datasets/rosalind_cons.txt"

  def getData(isPractice: Boolean): List[Dna] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val dnaStrings: List[Fasta] = readFastaSequences(data, isLineSeparated = false)
    for { Fasta(_, dna) <- dnaStrings } yield Dna(dna)
  }

  def calcProfileAndConsensus(sequences: List[Dna]): (List[List[Int]], Dna) = {
    val columnCounts: List[Map[DnaNucleotide, Int]] = calcColumnCountsOfDnaSequences(sequences)
    val consensus: Dna = Dna(columnCounts.map(_.maxBy(_._2)._1))
    val profile: List[List[Int]] = calcProfileFromColumnCounts(columnCounts)
    (profile, consensus)
  }

  def calcColumnCountsOfDnaSequences(sequences: List[Dna]): List[Map[DnaNucleotide, Int]] =
    sequences
      .map(_.sequence.toList)
      .transpose
      .map(_.groupBy(identity).map{ case (nucleotide, counterlist) => (nucleotide, counterlist.length) })

  def calcProfileFromColumnCounts(columnCounts: List[Map[DnaNucleotide, Int]]): List[List[Int]] =
    (for { column <- columnCounts } yield convertDNAMapToList(column)).transpose

  def transformOutput(profile: List[List[Int]]): List[List[String]] = {
    val nucleotides: List[Char] = List('A', 'C', 'G', 'T')
    for { (line, ix) <- profile.zipWithIndex } yield List(nucleotides(ix).toString + ": " + line.mkString(" "))
  }

  def main(args: Array[String]): Unit = {
    val sequences: List[Dna] = getData(isPractice = false)
    val (profile, consensus): (List[List[Int]], Dna) = calcProfileAndConsensus(sequences)
    val printableProfile: List[List[String]] = transformOutput(profile)
    writeListOfListsAsStringsToFile(List(consensus.toString) :: printableProfile)
  }

}
