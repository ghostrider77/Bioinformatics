package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/rear/
  */

object ReversalDistance {
  type Permutation = List[Int]

  object SampleData {
    val sample: List[String] =
      List(
        "1 2 3 4 5 6 7 8 9 10",
        "3 1 5 2 7 4 9 6 10 8",
        "",
        "3 10 8 2 5 4 7 1 6 9",
        "5 2 3 1 7 4 10 8 6 9",
        "",
        "8 6 7 9 4 1 3 10 2 5",
        "8 2 7 6 9 1 5 3 10 4",
        "",
        "3 9 10 4 1 8 6 7 5 2",
        "2 9 8 5 1 7 3 4 6 10",
        "",
        "1 2 3 4 5 6 7 8 9 10",
        "1 2 3 4 5 6 7 8 9 10"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_rear.txt"

  def readPermutations(data: List[String]): List[(Permutation, Permutation)] =
    data.grouped(3).map(triple => (convertStringToIntList(triple.head), convertStringToIntList(triple(1)))).toList

  def getData(isPractice: Boolean): List[(Permutation, Permutation)] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    readPermutations(data)
  }

  def calcNumberOfBreakPoints(perm: Permutation): Int = {
    val consecutiveElements: Iterator[List[Int]] = perm.sliding(2)
    @tailrec
    def loop(nrBreakPoints: Int): Int = {
      if (!consecutiveElements.hasNext) nrBreakPoints
      else {
        val List(number1, number2): List[Int] = consecutiveElements.next()
        if (math.abs(number1 - number2) != 1) loop(nrBreakPoints + 1)
        else loop(nrBreakPoints)
      }
    }
    val numberOfBreakPoints: Int = loop(0)
    val leftEndCorection: Int = if (perm.head != 1) 1 else 0
    val rightEndCorection: Int = if (perm.last != perm.length) 1 else 0
    numberOfBreakPoints + leftEndCorection + rightEndCorection
  }

  def keepNewPermutationsWithMaximumBreakPointReduction(newNeighbours: Set[(Permutation, Int)]): Set[Permutation] = {
    val maximalBreakPointReduction: Int =
      newNeighbours.foldLeft(0){ case (acc, (_, breakPointDifference)) => acc max breakPointDifference }
    for {
      (perm, breakPointDifference) <- newNeighbours
      if breakPointDifference == maximalBreakPointReduction
    } yield perm
  }

  def calcPermutationsWithMaximalBreakpointReduction(reversalNeighbourhood: Set[Permutation]): Set[Permutation] = {
    val newNeighbours: Set[(Permutation, Int)] =
      for {
        perm <- reversalNeighbourhood
        numberOfBreakPoints: Int = calcNumberOfBreakPoints(perm)
        Vector(ix1, ix2) <- perm.indices.combinations(2)
      } yield {
        val reversedPerm: Permutation = perm.take(ix1) ::: perm.slice(ix1, ix2 + 1).reverse ::: perm.drop(ix2 + 1)
        val breakPointReduction: Int = numberOfBreakPoints - calcNumberOfBreakPoints(reversedPerm)
        (reversedPerm, breakPointReduction)
      }
    keepNewPermutationsWithMaximumBreakPointReduction(newNeighbours)
  }

  def calcReversalDistance(perm: Permutation): Int = {
    val identity: Permutation = (1 to perm.length).toList
    @tailrec
    def loop(reversalNeighbourhood: Set[Permutation], reversalDistance: Int): Int = {
      if (reversalNeighbourhood.contains(identity)) reversalDistance
      else {
        val updatedNeighbourhood: Set[Permutation] =
          calcPermutationsWithMaximalBreakpointReduction(reversalNeighbourhood)
        loop(updatedNeighbourhood, reversalDistance + 1)
      }
    }

    loop(Set(perm), 0)
  }

  def calcReversalDistanceOfPermutationPairs(permutationPairs: List[(Permutation, Permutation)]): List[Int] =
    for { (perm1, perm2) <- permutationPairs } yield {
      val perm2Inverse: Permutation = perm2.indices.map(ix => perm2.indexOf(ix + 1) + 1).toList
      val perm1Transformed: Permutation = perm1.map(elem => perm2Inverse(elem - 1))
      calcReversalDistance(perm1Transformed)
    }

  def main(args: Array[String]): Unit = {
    val permutationPairs: List[(Permutation, Permutation)] = getData(isPractice = false)
    val result: List[Int] = calcReversalDistanceOfPermutationPairs(permutationPairs)
    writeListAsStringToFile(result)
  }

}
