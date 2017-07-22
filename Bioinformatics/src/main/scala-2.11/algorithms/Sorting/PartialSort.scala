package algorithms.Sorting

/**
  * problem description: http://rosalind.info/problems/ps/
  */

object PartialSort {

  object SampleData {
    val sample: List[String] =
      List(
        "10",
        "4 -6 7 8 -9 100 12 13 56 17",
        "3"
      )
  }

  import SampleData.sample
  import algorithms.Datastructures.{Heap, MinHeap}
  import algorithms.Utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}

  val inputFileName: String = "/algorithms/datasets/rosalind_ps.txt"

  def getData(isPractice: Boolean): (List[Int], Int) = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    val lst: List[Int] = convertStringToIntList(data(1))
    val k: Int = data(2).toInt
    (lst, k)
  }

  def main(args: Array[String]): Unit = {
    val (array, k): (List[Int], Int) = getData(isPractice = false)
    val heap = new Heap(array, heapType = MinHeap)
    val result: List[Int] = heap.partialSort(k).reverse.toList
    writeListAsStringToFile(result)
  }

}
