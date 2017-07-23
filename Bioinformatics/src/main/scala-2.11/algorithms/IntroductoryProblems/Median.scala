package algorithms.IntroductoryProblems

/**
  * problem description: http://rosalind.info/problems/med/
  */

object Median {

  object SampleData {
    val sample: List[String] =
      List(
        "11",
        "2 36 5 21 8 13 11 20 5 4 1",
        "8"
      )
  }

  import scala.annotation.tailrec
  import SampleData.sample
  import utils.UtilityFunctions.{convertStringToIntList, readInputData}

  val inputFileName: String = "/algorithms/datasets/rosalind_med.txt"

  def getData(isPractice: Boolean): (Vector[Int], Int) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val array: Vector[Int] = convertStringToIntList(data(1)).toVector
    val k: Int = data(2).toInt
    (array, k)
  }

  def threeWayPartition(array: Vector[Int], pivot: Int): (Vector[Int], Int, Int) = {

    @tailrec
    def loop(xs: Vector[Int],
             left: Vector[Int],
             right: Vector[Int],
             firstEqualIndex: Int,
             lastEqualIndex: Int): (Vector[Int], Vector[Int], Int, Int) = xs match {
      case Vector() => (left, right, firstEqualIndex, lastEqualIndex)
      case x +: xss =>
        if (x < pivot) loop(xss, x +: left, right, firstEqualIndex + 1, lastEqualIndex + 1)
        else if (x > pivot) loop(xss, left, x +: right, firstEqualIndex, lastEqualIndex)
        else loop(xss, left, right, firstEqualIndex, lastEqualIndex + 1)
    }

    val (less, greater, firstEqualIndex, lastEqualIndex): (Vector[Int], Vector[Int], Int, Int) =
      loop(array, Vector(), Vector(), 0, -1)
    val partitionedArray: Vector[Int] =
      less ++ Vector.fill[Int](lastEqualIndex - firstEqualIndex + 1)(pivot) ++ greater
    (partitionedArray, firstEqualIndex, lastEqualIndex)
  }

  def getKthSmallestElement(array: Vector[Int], k: Int): Int = {
    val rnd = scala.util.Random
    rnd.setSeed(2112L)

    @tailrec
    def loop(vector: Vector[Int], kPrime: Int): Int = {
      val ix: Int = rnd.nextInt(vector.length)
      val pivot: Int = vector(ix)
      val (partitionedArray, startIndex, lastIndex): (Vector[Int], Int, Int) = threeWayPartition(vector, pivot)
      if (kPrime <= startIndex) loop(partitionedArray.take(startIndex), kPrime)
      else if (startIndex < kPrime && kPrime <= lastIndex + 1) pivot
      else loop(partitionedArray.drop(lastIndex + 1), kPrime - lastIndex - 1)
    }

    loop(array, k)
  }

  def main(args: Array[String]): Unit = {
    val (array, k): (Vector[Int], Int) = getData(isPractice = false)
    val result: Int = getKthSmallestElement(array, k)
    println(result)
  }

}
