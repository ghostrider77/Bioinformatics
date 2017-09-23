package stronghold.graphs

/**
  * problem description: http://rosalind.info/problems/long/
  */

object GenomeAssemblyShortestSuperstring {

  object SampleData {
    val sample: List[String] =
      List(
        ">Rosalind_56",
        "ATTAGACCTG",
        ">Rosalind_57",
        "CCTGCCGGAA",
        ">Rosalind_58",
        "AGACCTGCCG",
        ">Rosalind_59",
        "GCCGGAATAC"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{Fasta, readInputData, readFastaSequences, writeListAsStringToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_long.txt"

  def getData(isPractice: Boolean): List[Fasta] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readFastaSequences(data, isLineSeparated = false)
  }

  def createOverlapGraph(dnaStrings: List[Fasta], commonLength: Int): Map[String, List[String]] = {
    val prefixes: Map[String, List[String]] =
      (for {
        Fasta(id, sequence) <- dnaStrings
        k <- (commonLength / 2) + 1 to commonLength
      } yield (sequence.take(k), id)).groupBy(_._1).mapValues(_.map(_._2))

    (for {
      Fasta(id, sequence) <- dnaStrings
      k <- 0 until math.ceil(commonLength / 2).toInt
      suffix: String = sequence.drop(k)
      matchingPrefixIds: List[String] = prefixes.getOrElse(suffix, Nil).filterNot(_ == id)
      if matchingPrefixIds.nonEmpty
    } yield id -> matchingPrefixIds).toMap
  }

  def findSourceNode(graph: Map[String, List[String]]): Option[String] =
    graph.keysIterator.find(keyNode => !graph.valuesIterator.flatten.contains(keyNode))

  def getLengthOfLongestMatchingPrefix(assembledString: String, string: String, length: Int): Int = {
    @tailrec
    def loop(k: Int): Int = {
      if (k <= length / 2 || assembledString.takeRight(k) == string.take(k)) k
      else loop(k - 1)
    }

    loop(length)
  }

  def assembleShortestSuperstring(dnaStrings: List[Fasta],
                                  commonLength: Int,
                                  graph: Map[String, List[String]],
                                  firstNode: String): String = {
    val dnaStringsMap: Map[String, String] = dnaStrings.map{ case Fasta(name, string) => name -> string }.toMap
    @tailrec
    def loop(superString: String, currentNode: String): String = {
      graph.get(currentNode) match {
        case None => superString
        case Some(neighbours) =>
          val neighbourName: String = neighbours.head
          val neighbourString: String = dnaStringsMap(neighbourName)
          val ix: Int = getLengthOfLongestMatchingPrefix(superString, neighbourString, commonLength)
          loop(superString + neighbourString.drop(ix), neighbourName)
      }
    }
    loop(dnaStringsMap(firstNode), firstNode)
  }

  def findShortestSuperstring(dnaStrings: List[Fasta]): String = {
    val commonLength: Int = dnaStrings.head.string.length
    val graph: Map[String, List[String]] = createOverlapGraph(dnaStrings, commonLength)
    val firstNode: String = findSourceNode(graph).get
    assembleShortestSuperstring(dnaStrings, commonLength, graph, firstNode)
  }

  def main(args: Array[String]): Unit = {
    val dnaStrings: List[Fasta] = getData(isPractice = false)
    val result: String = findShortestSuperstring(dnaStrings)
    writeListAsStringToFile(result.toList, sep="")
  }
}
