package stronghold.phylogeny

import org.scalatest.{FreeSpec, Matchers, Inspectors}

class PhylogenySuite extends FreeSpec with Matchers with Inspectors {

  object Constants {
    val absoluteTolerance: Double = 0.001
  }

  "CountingPhylogeneticAncestors" - {
    import CountingPhylogeneticAncestors.{getData, numberOfInnerNodesInUnrootedBinaryTree}

    "should calculate the number of internal nodes of any unrooted binary tree having n leaves" in {
      val numberOfLeaves: Int = getData(isPractice = true)
      numberOfInnerNodesInUnrootedBinaryTree(numberOfLeaves) shouldEqual 2
    }
  }

  "CreatingADistanceMatrix" - {
    import CreatingADistanceMatrix.{getData, calcDistanceMatrix}
    import utils.Dna
    import Constants.absoluteTolerance

    "Should retrieve the distance matrix corresponding to the p-distance on the given string" in {
      val dnas: List[Dna] = getData(isPractice = true)
      val expectedDistanceMatrix: Array[Array[Double]] =
        Array(
          Array(0.0, 0.4, 0.1, 0.1),
          Array(0.4, 0.0, 0.4, 0.3),
          Array(0.1, 0.4, 0.0, 0.2),
          Array(0.1, 0.3, 0.2, 0.0)
        )
      val calculatedDistanceMatrix: Array[Array[Double]] = calcDistanceMatrix(dnas)
      forAll(expectedDistanceMatrix.flatten.toList.zip(calculatedDistanceMatrix.flatten.toList)) {
        case (expected, calculated) => expected shouldBe (calculated +- absoluteTolerance)
      }
    }
  }
}
