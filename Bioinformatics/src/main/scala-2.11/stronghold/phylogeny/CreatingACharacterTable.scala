package stronghold.phylogeny

/**
  * problem description: http://rosalind.info/problems/ctbl/
  */

object CreatingACharacterTable {

  type Character = List[Boolean]

  object SampleData {
    val sample: List[String] = List("(dog,((elephant,mouse),robot),cat);")
  }

  import scala.annotation.tailrec
  import scala.util.matching.Regex
  import SampleData.sample
  import utils.UtilityFunctions.{readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_ctbl.txt"

  private val nonRelevantLetters = new Regex("""[^0-9a-zA-Z_]+""")
  private val oneSpace: String = " "

  def getData(isPractice: Boolean): String = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    data.head
  }

  def getAlphabeticalListOfTaxa(newickTreeString: String): List[String] =
    nonRelevantLetters.replaceAllIn(newickTreeString, oneSpace).trim().split(oneSpace).toList.sorted

  def getSubtreesByLevel(newickTreeString: String): Map[Int, List[String]] = {
    val length: Int = newickTreeString.length

    @tailrec
    def loop(ix: Int,
             depth: Int,
             indicesOfOpenParens: List[Int],
             subtrees: Map[Int, List[String]]): Map[Int, List[String]] = {
      if (ix >= length) subtrees
      else {
        val letter: Char = newickTreeString(ix)
        if (letter == '(') loop(ix + 1, depth + 1, ix :: indicesOfOpenParens, subtrees)
        else if (letter == ')') {
          val sub: String = newickTreeString.substring(indicesOfOpenParens.head + 1, ix)
          val updatedSubtrees: Map[Int, List[String]] =
            subtrees.updated(depth - 1, sub :: subtrees.getOrElse(depth - 1, Nil))
          loop(ix + 1, depth - 1, indicesOfOpenParens.tail, updatedSubtrees)
        }
        else loop(ix + 1, depth, indicesOfOpenParens, subtrees)
      }
    }

    loop(0, 0, Nil, Map())
  }

  def createCharacterTableFromNewickTree(newickTreeString: String): List[Character] = {
    val taxa: List[String] = getAlphabeticalListOfTaxa(newickTreeString)
    val subtrees: Map[Int, List[String]] = getSubtreesByLevel(newickTreeString)
    val maximumDepth: Int = subtrees.keysIterator.max
    val numberOfTaxa: Int = taxa.length

    (for {
      depth <- 1 to maximumDepth
      properSubtreesAtGivenLevel: List[String] = subtrees(depth)
      newickString <- properSubtreesAtGivenLevel
      indices: Set[Int] = (for { taxon <- getAlphabeticalListOfTaxa(newickString) } yield taxa.indexOf(taxon)).toSet
    } yield (for { ix <- 0 until numberOfTaxa } yield indices.contains(ix)).toList).toList
  }

  def createPrintableCharacterString(character: Character): String = character.map(if (_) 1 else 0).mkString("")

  def main(args: Array[String]): Unit = {
    val newickTreeString: String = getData(isPractice = false)
    val result: List[Character] = createCharacterTableFromNewickTree(newickTreeString)
    writeListOfListsAsStringsToFile(result.map(character => List(createPrintableCharacterString(character))))
  }

}
