package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/sort/
  */

object SortingByReversals {
  type Permutation = List[Int]

  object SampleData {
    val sample: List[String] =
      List(
        "1 2 3 4 5 6 7 8 9 10",
        "1 8 9 3 2 7 6 5 4 10"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_sort.txt"

  case class PermutationWithReversal(perm: Permutation, reversedPerm: Permutation, breakPointReduction: Int)

  def getData(isPractice: Boolean): List[Permutation] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    for { line <- data } yield convertStringToIntList(line)
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
    val leftEndCorrection: Int = if (perm.head != 1) 1 else 0
    val rightEndCorrection: Int = if (perm.last != perm.length) 1 else 0
    numberOfBreakPoints + leftEndCorrection + rightEndCorrection
  }

  def keepNewPermutationsWithMaximumBreakPointReduction(
    newNeighbours: Set[PermutationWithReversal]): (Set[Permutation], Map[Permutation, Permutation]) = {
    val maximalBreakPointReduction: Int = newNeighbours.map(_.breakPointReduction).max
    val newPermutations: Set[Permutation] = for {
      PermutationWithReversal(_, reversedPerm, breakPointDifference) <- newNeighbours
      if breakPointDifference == maximalBreakPointReduction
    } yield reversedPerm
    val backtrackingInfo: Map[Permutation, Permutation] = (for {
      PermutationWithReversal(perm, reversedPerm, _) <- newNeighbours
      if newPermutations.contains(reversedPerm)
    } yield reversedPerm -> perm).toMap

    (newPermutations, backtrackingInfo)
  }

  def calcPermutationsWithMaximalBreakpointReduction(
    reversalNeighbourhood: Set[Permutation]): (Set[Permutation], Map[Permutation, Permutation]) = {
    val newNeighbours: Set[PermutationWithReversal] =
      for {
        perm <- reversalNeighbourhood
        numberOfBreakPoints: Int = calcNumberOfBreakPoints(perm)
        Vector(ix1, ix2) <- perm.indices.combinations(2)
      } yield {
        val reversedPerm: Permutation = perm.take(ix1) ::: perm.slice(ix1, ix2 + 1).reverse ::: perm.drop(ix2 + 1)
        val breakPointReduction: Int = numberOfBreakPoints - calcNumberOfBreakPoints(reversedPerm)
        PermutationWithReversal(perm, reversedPerm, breakPointReduction)
      }
    keepNewPermutationsWithMaximumBreakPointReduction(newNeighbours)
  }

  def calcReversalDistance(perm: Permutation, identity: Permutation): (Int, Map[Permutation, Permutation]) = {
    @tailrec
    def loop(reversalNeighbourhood: Set[Permutation],
             reversalDistance: Int,
             backtrack: Map[Permutation, Permutation]): (Int, Map[Permutation, Permutation]) = {
      if (reversalNeighbourhood.contains(identity)) (reversalDistance, backtrack)
      else {
        val (updatedNeighbourhood, backtrackingInfo): (Set[Permutation], Map[Permutation, Permutation]) =
          calcPermutationsWithMaximalBreakpointReduction(reversalNeighbourhood)
        loop(updatedNeighbourhood, reversalDistance + 1, backtrack ++ backtrackingInfo)
      }
    }

    loop(Set(perm), 0, Map())
  }

  def getReversals(backtrack: Map[Permutation, Permutation], identity: Permutation): List[(Int, Int)] = {
    @tailrec
    def loop(perm: Permutation, reversals: List[(Int, Int)]): List[(Int, Int)] = backtrack.get(perm) match {
      case None => reversals
      case Some(parentPerm) =>
        val first: Int = perm.zip(parentPerm).indexWhere{ case (elem1, elem2) => elem1 != elem2 } + 1
        val second: Int = perm.zip(parentPerm).lastIndexWhere{ case (elem1, elem2) => elem1 != elem2 } + 1
        loop(parentPerm, (first, second) :: reversals)
    }

    loop(identity, Nil)
  }

  def sortingByReversals(perm1: Permutation, perm2: Permutation): (Int, List[(Int, Int)]) = {
    val perm2Inverse: Permutation = perm2.indices.map(ix => perm2.indexOf(ix + 1) + 1).toList
    val perm1Transformed: Permutation = perm1.map(elem => perm2Inverse(elem - 1))
    val identity: Permutation = (1 to perm1.length).toList
    val (distance, backtrack): (Int, Map[Permutation, Permutation]) = calcReversalDistance(perm1Transformed, identity)
    val reversals: List[(Int, Int)] = getReversals(backtrack, identity)
    (distance, reversals)
  }

  def main(args: Array[String]): Unit = {
    val List(perm1, perm2): List[Permutation] = getData(isPractice = true)
    val (distance, reversals): (Int, List[(Int, Int)]) = sortingByReversals(perm1, perm2)
    writeListOfListsAsStringsToFile(List(distance) :: reversals.map{ case (ix1, ix2) => List(ix1, ix2) })
  }

}
