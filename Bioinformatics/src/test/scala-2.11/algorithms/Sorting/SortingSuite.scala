package algorithms.Sorting

import org.scalatest.{FreeSpec, Matchers}

class SortingSuite extends FreeSpec with Matchers {

  object HeapTest {
    import algorithms.Datastructures.{HeapType, MaxHeap, MinHeap}

    def checkHeapProperty(heap: Array[Int], heapType: HeapType): Boolean = {
      val compare: (Int, Int) => Boolean = heapType match {
        case MaxHeap => (parentValue, childValue) => parentValue > childValue
        case MinHeap => (parentValue, childValue) => parentValue < childValue
      }

      (2 to heap.length).forall(ix => !compare(heap(ix - 1), heap(ix / 2 - 1)))
    }
  }

  "InsertionSort" - {
    import InsertionSort.{getData, calcNumberOfSwaps}

    "should calculate the number of swaps in the sample problem correctly" in {
      val (n, array): (Int, Array[Int]) = getData(isPractice = true)
      calcNumberOfSwaps(array, n) shouldEqual 12
    }
  }

  "MergeTwoSortedArrays" - {
    import MergeTwoSortedArrays.{getData, mergeSortedArrays}

    "should merge the sample sorted arrays correctly" in {
      val (first, second): (List[Int], List[Int]) = getData(isPractice = true)
      mergeSortedArrays(first, second) shouldEqual List(-5, 2, 4, 10, 11, 12, 18)
    }

    "should merge non-overlapping arrays" in {
      val first: List[Int] = List(1, 2, 4)
      val second: List[Int] = List(10, 20)
      val concatenatedList: List[Int] = first ::: second
      mergeSortedArrays(first, second) shouldEqual concatenatedList
      mergeSortedArrays(second, first) shouldEqual concatenatedList
    }
  }

  "BuildAHeap" - {
    import algorithms.Datastructures.{Heap, MaxHeap}
    import BuildAHeap.getData
    import HeapTest.checkHeapProperty

    "should build a max-heap from the sample data" in {
      val array: List[Int] = getData(isPractice = true)
      val heap = new Heap(array, heapType = MaxHeap)
      checkHeapProperty(heap.heapify(), heapType = MaxHeap) shouldBe true
    }

    "should build a max-heap on a larger array" in {
      val array: List[Int] =
        List(1, 3, 5, 7, 2, 4, 9, 12, 8, 6, 30, 11, 10, 0, 23, 21, 20, 19, 33, 34, 35, 36, 37, 38, 39, 40)
      val heap = new Heap(array, heapType = MaxHeap)
      checkHeapProperty(heap.heapify(), heapType = MaxHeap) shouldBe true
    }
  }

  "MergeSort" - {
    import MergeSort.{getData, mergeSort}

    "should sort the sample array correctly" in {
      val array: List[Int] = getData(isPractice = true)
      mergeSort(array) shouldBe sorted
    }
  }

  "CountInversions" - {
    import CountInversions.{getData, countInversions}

    "should count the number of inversions for the sample problem" in {
      val array: List[Long] = getData(isPractice = true)
      countInversions(array)._2 shouldEqual 2
    }

    "should have zero inversions in a sorted list" in {
      val array: List[Long] = List(1, 4, 8, 9, 11, 16)
      countInversions(array)._2 shouldEqual 0
    }

    "should have n over 2 inversions in a reverse sorted list" in {
      val array: List[Long] = List(1, 4, 8, 9, 11, 16).reverse.map(_.toLong)
      val n: Int = array.length
      val nOver2: Int = n * (n - 1) / 2
      countInversions(array)._2 shouldEqual nOver2
    }
  }

  "TwoWayPartition" - {
    import TwoWayPartition.{getData, partition}

    "should partition the array in the sample problem" in {
      val array: List[Int] = getData(isPractice = true)
      val pivot: Int = array.head
      val numberOfElementsLessOrEqualThanPivot: Int = array.count(_ <= pivot)

      val partitionedArray: List[Int] = partition(array.tail, array.head)
      val (first, second): (List[Int], List[Int]) = partitionedArray.splitAt(numberOfElementsLessOrEqualThanPivot)

      all(first) should be <= pivot
      all(second) should be > pivot
    }
  }

  "HeapSort" - {
    import algorithms.Datastructures.{Heap, MaxHeap, MinHeap}
    import HeapSort.getData

    "should sort the sample array" in {
      val array: List[Int] = getData(isPractice = true)
      val heap = new Heap(array, heapType = MaxHeap)
      heap.heapSort() shouldBe sorted
    }

    "should reverse-sort the sample array when a min-heap is used" in {
      val array: List[Int] = getData(isPractice = true)
      val heap = new Heap(array, heapType = MinHeap)
      heap.heapSort().reverse shouldBe sorted
    }
  }

  "PartialSort" - {
    import algorithms.Datastructures.{Heap, MinHeap}
    import PartialSort.getData

    "should partially sort the sample array" in {
      val (array, k): (List[Int], Int) = getData(isPractice = true)
      val heap = new Heap(array, heapType = MinHeap)
      heap.partialSort(k).reverse shouldEqual List(-9, -6, 4)
    }

    "should retrieve the first k elements of a sorted array" in {
      val array: List[Int] = List(4, 2, 9, 0, 3, 1, 1, 8)
      val length: Int = array.length
      val minheap = new Heap(array, heapType = MinHeap)
      minheap.partialSort(3).reverse shouldEqual List(0, 1, 1)
      minheap.partialSort(length).reverse shouldEqual array.sorted
      minheap.partialSort(length - 1).reverse shouldEqual array.sorted.dropRight(1)
    }

  }

  "ThreeWayPartition" - {
    import ThreeWayPartition.{getData, partition3}

    "should partition the array in the sample problem" in {
      val array: List[Int] = getData(isPractice = true)
      val pivot: Int = array.head
      val numberOfElementsLessThanPivot: Int = array.count(_ < pivot)
      val numberOfElementsGreaterThanPivot: Int = array.count(_ > pivot)

      val partitionedArray: List[Int] = partition3(array.tail, array.head)
      val less: List[Int] = partitionedArray.take(numberOfElementsLessThanPivot)
      val greater: List[Int] = partitionedArray.takeRight(numberOfElementsGreaterThanPivot)

      all(less) should be < pivot
      all(greater) should be > pivot
    }
  }

  "QuickSort" - {
    import QuickSort.{getData, quicksort}

    "should sort the sample array" in {
      val array: Vector[Int] = getData(isPractice = true)
      quicksort(array) shouldBe sorted
    }
  }

}
