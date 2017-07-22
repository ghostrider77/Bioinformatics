package algorithms.Sorting

/**
  * problem description: http://rosalind.info/problems/qs/
  */

object QuickSort {

  object SampleData {
    val sample: List[String] =
      List(
        "7",
        "5 -2 4 7 8 -10 11"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import algorithms.Utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}

  val inputFileName: String = "/algorithms/datasets/rosalind_qs.txt"

  def getData(isPractice: Boolean): Vector[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data(1)).toVector
  }

  def threeWayPartition(array: Vector[Int], pivot: Int): (Vector[Int], Vector[Int], Int) = {

    @tailrec
    def loop(xs: Vector[Int],
             left: Vector[Int],
             right: Vector[Int],
             equalToPivot: Int): (Vector[Int], Vector[Int], Int) = xs match {
      case Vector() => (left, right, equalToPivot)
      case x +: xss =>
        if (x < pivot) loop(xss, x +: left, right, equalToPivot)
        else if (x > pivot) loop(xss, left, x +: right, equalToPivot)
        else loop(xss, left, right, equalToPivot + 1)
    }

    loop(array, Vector(), Vector(), 0)
  }

  def quicksort(array: Vector[Int]): Vector[Int] = {
    val rnd = scala.util.Random
    rnd.setSeed(2112L)

    def qs(xs: Vector[Int]): Vector[Int] = {
      val length: Int = xs.length
      if (length <= 1) xs
      else {
        val ix: Int = rnd.nextInt(length)
        val pivot: Int = xs(ix)
        val (less, greater, numberOfEqualElems): (Vector[Int], Vector[Int], Int) = threeWayPartition(xs, pivot)
        qs(less) ++ Vector.fill[Int](numberOfEqualElems)(pivot) ++ qs(greater)
      }
    }

    qs(array)
  }

  def main(args: Array[String]): Unit = {
    val array: Vector[Int] = getData(isPractice = false)
    val result: Vector[Int] = quicksort(array)
    writeListAsStringToFile(result.toList)
  }

}
