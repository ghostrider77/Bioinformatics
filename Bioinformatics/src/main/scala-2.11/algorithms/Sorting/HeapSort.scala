package algorithms.Sorting

/**
  * problem description: http://rosalind.info/problems/hea/
  */

object HeapSort {

  object SampleData {
    val sample: List[String] =
      List(
        "9",
        "2 6 7 1 3 5 4 8 9"
      )
  }

  import SampleData.sample
  import algorithms.Datastructures.{Heap, MaxHeap}
  import algorithms.Utils.UtilityFunctions.{convertStringToIntList, readInputData, writeListAsStringToFile}

  val inputFileName: String = "/algorithms/datasets/rosalind_hs.txt"

  def getData(isPractice: Boolean): List[Int] = {
    val data: List[String] = if (isPractice) sample else readInputData(inputFileName)
    convertStringToIntList(data(1))
  }

  def main(args: Array[String]): Unit = {
    val array: List[Int] = getData(isPractice = false)
    val heap = new Heap(array, heapType = MaxHeap)
    val result: List[Int] = heap.heapSort().toList
    writeListAsStringToFile(result)
  }

}
