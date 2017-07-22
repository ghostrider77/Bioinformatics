package algorithms.IntroductoryProblems

import org.scalatest.{FreeSpec, Matchers}

class IntroductoryProblemSuite extends FreeSpec with Matchers {

  "Fibonacci" - {
    import Fibonacci.{calcFibonacciNumber, getData}

    "should calculate the Fibonacci numbers for n = 0 and n = 1 correctly" in {
      val f0: Long = calcFibonacciNumber(0)
      val f1: Long = calcFibonacciNumber(1)

      f0 shouldEqual 0
      f1 shouldEqual 1
    }

    "should solve the sample problem correctly" in {
      val n: Int = getData(isPractice = true)
      calcFibonacciNumber(n) shouldEqual 8
    }
  }

  "BinarySearch" - {
    import BinarySearch.{findIndicesOfElems, getData, changeToOneBasedIndexing}

    "should return the indices of elements" in {
      val sortedArray: Vector[Int] = Vector(1, 3, 4, 4, 4, 7, 9, 9, 14)
      val elems: List[Int] = List(9, 5, 4, 14, 20, 0)
      val indices: List[Int] =
        findIndicesOfElems(sortedArray, sortedArray.length, elems)
      val indicesOfElemsNotPresentInArray: List[Int] = List(1, 4, 5)

      indicesOfElemsNotPresentInArray.map(ix => indices(ix)) should have length 3
      indicesOfElemsNotPresentInArray.map(ix => indices(ix)) should contain only (-1)

      List(6, 7) should contain(indices(0))
      List(2, 3, 4) should contain(indices(2))
      8 shouldEqual indices(3)
    }

    "should solve the sample problem correctly" in {
      val (n, array, elems): (Int, Vector[Int], List[Int]) =
        getData(isPractice = true)
      val result: List[Int] =
        findIndicesOfElems(array, n, elems).map(changeToOneBasedIndexing)
      result shouldEqual List(4, 1, -1, -1, 4, 2)
    }
  }

  "MajorityElement" - {
    import MajorityElement.{getData, calcMajorityElements}

    "should return the majority elements for the sample problems correctly" in {
      val (n, arrays): (Int, List[List[Int]]) = getData(isPractice = true)
      calcMajorityElements(arrays, n) shouldEqual List(5, 7, -1, -1)
    }

  }

  "TwoSum" - {
    import TwoSum.{getData, find2SumIndices, correctToOneBasedIndexing}

    "should return the indices for the sample problems correctly" in {
      val arrays: List[List[Int]] = getData(isPractice = true)
      find2SumIndices(arrays).map(correctToOneBasedIndexing) shouldEqual
        List(List(-1), List(2, 4), List(-1), List(1, 3))
    }
  }

  "ThreeSum" - {
    import ThreeSum.{getData, find3SumIndices, correctToOneBasedIndexing}

    "should return the indices for the sample problem correctly" in {
      val arrays: List[List[Int]] = getData(isPractice = true)
      find3SumIndices(arrays).map(correctToOneBasedIndexing) shouldEqual
        List(List(-1), List(1, 2, 4), List(1, 2, 3), List(-1))
    }
  }

  "Median" - {
    import Median.{getData, getKthSmallestElement}

    "should return the kth smallest element for the sample problem" in {
      val (array, k): (Vector[Int], Int) = getData(isPractice = true)
      getKthSmallestElement(array, k) shouldEqual 13
    }

    "should return the smallest element of an array when k is one" in {
      val array: Vector[Int] = Vector(2, 7, 2, 2, 1, 3, 8)
      getKthSmallestElement(array, k = 1) shouldEqual array.min
    }

  }

}
