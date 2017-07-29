package stronghold.alignment

import org.scalatest.{FreeSpec, Matchers}

class AlignmentSuite extends FreeSpec with Matchers {

  "CountingPointMutations" - {
    import CountingPointMutations.{getData, calcHammingDistance}

    "should compute Hamming distance of two strings of equal length" in {
      val (s1, s2): (String, String) = getData(isPractice = true)
      calcHammingDistance(s1.toList, s2.toList) shouldEqual 7
    }
  }

}
