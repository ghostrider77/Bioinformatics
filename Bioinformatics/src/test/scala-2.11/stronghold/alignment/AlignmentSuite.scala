package stronghold.alignment

import org.scalatest.{FreeSpec, Matchers}
import utils.UtilityFunctions.Alignment

class AlignmentSuite extends FreeSpec with Matchers {

  private val absoluteTolerance: Double = 1e-03

  "CountingPointMutations" - {
    import CountingPointMutations.{getData, calcHammingDistance}

    "should compute Hamming distance of two strings of equal length" in {
      val (s1, s2): (String, String) = getData(isPractice = true)
      calcHammingDistance(s1.toList, s2.toList) shouldEqual 7
    }
  }

  "EditDistance" - {
    import EditDistance.{getData, calcLevenshteinDistance}

    "should calculate the edit distance of the sample strings" in {
      val List(string1, string2): List[String] = getData(isPractice = true)
      calcLevenshteinDistance(string1, string2) shouldEqual 5
    }

    "should calculate the edit distance between a string and the empty string" in {
      val s: String = "ABCDEF"
      calcLevenshteinDistance(s, "") shouldEqual s.length
    }
  }

  "EditDistanceAlignment" - {
    import EditDistanceAlignment.{getData, calcGlobalAlignmentUsingHammingDistance}

    "should calculate the global alignment score for the sample strings using Hamming distance scores" in {
      val List(string1, string2): List[String] = getData(isPractice = true)
      val result: Alignment = calcGlobalAlignmentUsingHammingDistance(string1, string2)
      result.alignmentScore shouldEqual 4
      result.alignedString1 shouldEqual "PRETTY--"
      result.alignedString2 shouldEqual "PR-TTEIN"
    }
  }

  "TransitionsAndTransversions" - {
    import TransitionsAndTransversions.{getData, calcTransitionTransversionRatio}
    import utils.Dna

    "should calculate the ratio of transitions and transversions" in {
      val List(dna1, dna2): List[Dna] = getData(isPractice = true)
      calcTransitionTransversionRatio(dna1, dna2) shouldBe (1.21428571429 +- absoluteTolerance)
    }
  }

}
