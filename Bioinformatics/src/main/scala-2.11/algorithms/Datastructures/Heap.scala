package algorithms.Datastructures

import scala.annotation.tailrec

sealed trait HeapType
case object MaxHeap extends HeapType
case object MinHeap extends HeapType

class Heap(lst: Seq[Int], heapType: HeapType) {
  import Heap.{insert, swapTwoElementsByIndex}

  private val length: Int = lst.length
  private val compare: (Int, Int) => Boolean = heapType match {
    case MaxHeap => (parentValue, childValue) => parentValue > childValue
    case MinHeap => (parentValue, childValue) => parentValue < childValue
  }

  def heapify(): Array[Int] = {
    val heap: Array[Int] = Array.fill(length)(0)
    for { (elem, ix) <- lst.view.zipWithIndex } insert(heap, elem, compare, ix)
    heap
  }

  def heapSort(): Array[Int] = {
    val heap: Array[Int] = heapify()
    for { lastIndex <- (length - 1) to 1 by -1 } {
      swapTwoElementsByIndex(heap, 0, lastIndex)
      restoreHeapProperty(heap, lastIndex)
    }
    heap
  }

  def partialSort(k: Int): Array[Int] = {
    if (k >= length) heapSort()
    else {
      val heap: Array[Int] = heapify()
      for { lastIndex <- (length - 1) to (length - k) by -1 } {
        swapTwoElementsByIndex(heap, 0, lastIndex)
        restoreHeapProperty(heap, lastIndex)
      }
      heap.takeRight(k)
    }
  }

  private def getIndexOfLargestChild(parentIx: Int, leftChildIx: Int, heap: Array[Int], length: Int): Int = {
    val rightChildIx: Int = leftChildIx + 1
    val changeWithLeft: Boolean = compare(heap(leftChildIx), heap(parentIx))
    val swapWithParentIx: Int = if (changeWithLeft) leftChildIx else parentIx
    val changeWithRight: Boolean = (rightChildIx < length) && compare(heap(rightChildIx), heap(swapWithParentIx))
    if (changeWithRight) rightChildIx else swapWithParentIx
  }

  private def restoreHeapProperty(heap: Array[Int], reducedLength: Int): Unit = {
    @tailrec
    def siftDown(parentIx: Int, leftChildIx: Int): Unit = {
      if (leftChildIx <= reducedLength - 1) {
        val childIx: Int = getIndexOfLargestChild(parentIx, leftChildIx, heap, reducedLength)
        if (childIx > parentIx) {
          swapTwoElementsByIndex(heap, childIx, parentIx)
          siftDown(childIx, 2 * childIx + 1)
        }
      }
    }

    siftDown(parentIx = 0, leftChildIx = 1)
  }

}

object Heap {

  private def getParentIndex(childIx: Int): Int =  0 max ((childIx - 1) / 2)

  private def swapTwoElementsByIndex(heap: Array[Int], ix: Int, jy: Int): Unit = {
    val elem: Int = heap(ix)
    heap(ix) = heap(jy)
    heap(jy) = elem
  }

  private def insert(heap: Array[Int], elem: Int, compare: (Int, Int) => Boolean, indexOfElem: Int): Unit = {
    heap(indexOfElem) = elem

    @tailrec
    def bubbleUp(childIx: Int, parentIx: Int): Unit = {
      if (compare(heap(childIx), heap(parentIx))) {
        swapTwoElementsByIndex(heap, childIx, parentIx)
        bubbleUp(parentIx, getParentIndex(parentIx))
      }
    }

    val indexOfParent: Int = getParentIndex(indexOfElem)
    bubbleUp(indexOfElem, indexOfParent)
  }

}
