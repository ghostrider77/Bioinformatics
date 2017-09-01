package stronghold.graphs

/**
  * problem description: http://rosalind.info/problems/grph/
  */

object OverlapGraphs {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_0498",
        "AAATAAA",
        ">Rosalind_2391",
        "AAATTTT",
        ">Rosalind_2323",
        "TTTTCCC",
        ">Rosalind_0442",
        "AAATCCC",
        ">Rosalind_5013",
        "GGGTGGG"
      )
  }

  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readInputData, readFastaSequences, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_grph.txt"

  def getData(isPractice: Boolean): List[Fasta] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readFastaSequences(data, isLineSeparated = false)
  }

  def createOverlapGraph(dnaStrings: List[Fasta], k: Int): Map[String, List[String]] = {
    val prefixes: Map[String, List[String]] =
      (for { Fasta(id, sequence) <- dnaStrings } yield (sequence.take(k), id)).groupBy(_._1).mapValues(_.map(_._2))

    (for {
      Fasta(id, sequence) <- dnaStrings
      kSuffix: String = sequence.takeRight(k)
      matchingPrefixIds: List[String] = prefixes.getOrElse(kSuffix, Nil).filterNot(_ == id)
      if matchingPrefixIds.nonEmpty
    } yield id -> matchingPrefixIds).toMap
  }

  def main(args: Array[String]): Unit = {
    val dnaStrings: List[Fasta] = getData(isPractice = false)
    val result: Map[String, List[String]] = createOverlapGraph(dnaStrings, k = 3)
    writeListOfListsAsStringsToFile(result.toList.flatMap{ case (id, lst) => lst.map(List(id, _)) })
  }
}
