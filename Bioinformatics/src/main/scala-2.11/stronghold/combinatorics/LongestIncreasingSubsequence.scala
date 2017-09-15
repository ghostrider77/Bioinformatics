package stronghold.combinatorics

/**
  * problem description: http://rosalind.info/problems/lgis/
  */

object LongestIncreasingSubsequence {

  object SampleData {
    val sample: List[String] =
      List(
        "5",
        "5 1 4 2 3"
      )
  }

  import scala.annotation.tailrec
  import scala.collection.mutable.ListBuffer
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListOfListsAsStringsToFile}

  val inputFileName: String = "/stronghold/datasets/rosalind_lgis.txt"

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data(1))
  }

  def findFirstStackWhereNumberIsLessThanTopElem(number: Int, stacks: ListBuffer[List[Int]]): Int =
    stacks.indexWhere(stack => number < stack.head)

  def putNumberOntoAStack(number: Int, stacks: ListBuffer[List[Int]]): Option[Int] = {
    val ix: Int = findFirstStackWhereNumberIsLessThanTopElem(number, stacks)
    if (ix == -1) {
      val lastStackTopElem: Int = stacks.last.head
      stacks += List(number)
      Some(lastStackTopElem)
    } else if (ix == 0) {
      stacks(ix) = number :: stacks(ix)
      None
    } else {
      val lastStackTopElem: Int = stacks(ix - 1).head
      stacks(ix) = number :: stacks(ix)
      Some(lastStackTopElem)
    }
  }

  def createPatienceSortingStacks(sequence: List[Int]): (Int, Map[Int, Int]) = {
    val stacks: ListBuffer[List[Int]] = ListBuffer(List(Int.MaxValue))
    val backtrack: Map[Int, Int] =
      (for {
        number <- sequence
        numberOnTopOfPreviousStack: Option[Int] = putNumberOntoAStack(number, stacks)
        if numberOnTopOfPreviousStack.nonEmpty
      } yield number -> numberOnTopOfPreviousStack.get).toMap
    val topRightmostNumber: Int = stacks.last.head
    (topRightmostNumber, backtrack)
  }

  def assembleSubsequence(startingNumber: Int, backtrack: Map[Int, Int]): List[Int] = {
    @tailrec
    def assemble(increasingSubsequence: List[Int], currentNumber: Int): List[Int] = {
      val extendedSequence: List[Int] = currentNumber :: increasingSubsequence
      backtrack.get(currentNumber) match {
        case None => extendedSequence
        case Some(previousNumber) => assemble(extendedSequence, previousNumber)
      }
    }
    assemble(Nil, startingNumber)
  }

  def calcLongestIncreasingSubsequence(sequence: List[Int]): List[Int] = {
    val (startingNumber, backtrack): (Int, Map[Int, Int]) = createPatienceSortingStacks(sequence)
    assembleSubsequence(startingNumber, backtrack)
  }

  def main(args: Array[String]): Unit = {
    val sequence: List[Int] = getData(isPractice = false)
    val longestIncreasingSubseq: List[Int] = calcLongestIncreasingSubsequence(sequence)
    val longestDecreasingSubseq: List[Int] = calcLongestIncreasingSubsequence(sequence.reverse).reverse
    writeListOfListsAsStringsToFile(List(longestIncreasingSubseq, longestDecreasingSubseq))
  }

}