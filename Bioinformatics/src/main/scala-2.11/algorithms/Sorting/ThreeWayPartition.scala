package algorithms.Sorting

/**
  * problem description: http://rosalind.info/problems/par3/
  */

object ThreeWayPartition {

  object SampleData {
    val sample: List[String] =
      List(
        "9",
        "4 5 6 4 1 2 5 7 4"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import algorithms.Utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}

  val inputFileName: String = "/algorithms/datasets/rosalind_par3.txt"

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data(1))
  }

  def partition3(array: List[Int], pivot: Int): List[Int] = {
    @tailrec
    def loop(xs: List[Int], less: List[Int], greater: List[Int], equal: List[Int]): (List[Int], List[Int], List[Int]) =
      xs match {
        case Nil => (less, greater, equal)
        case x :: xss =>
          if (x < pivot) loop(xss, x :: less, greater, equal)
          else if (x > pivot) loop(xss, less, x :: greater, equal)
          else loop(xss, less, greater, x :: equal)
      }

    val (less, greater, equal): (List[Int], List[Int], List[Int]) = loop(array, Nil, Nil, Nil)
    less ::: equal ::: greater
  }

  def main(args: Array[String]): Unit = {
    val array: List[Int] = getData(isPractice = false)
    val result: List[Int] = partition3(array.tail, array.head)
    writeListAsStringToFile(result)
  }

}
