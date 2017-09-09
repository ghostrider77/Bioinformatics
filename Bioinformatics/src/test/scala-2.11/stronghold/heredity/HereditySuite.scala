package stronghold.heredity

import org.scalatest.{FreeSpec, Matchers}

class HereditySuite extends FreeSpec with Matchers {

  private val absoluteTolerance: Double = 1e-03

  "MendelsFirstLaw" - {
    import MendelsFirstLaw.{getData, calcProbabilityChildPossessDominantAllele}

    "should calculate the probability that a child possesses a dominant allele" in {
      val List(d, h, r): List[Int] = getData(isPractice = true)
      calcProbabilityChildPossessDominantAllele(d, h, r) shouldBe (0.78333 +- absoluteTolerance)
    }
  }

  "CalculatingExpectedOffspring" - {
    import CalculatingExpectedOffspring.{getData, calcNumberOfOffspringsDisplayingDominantGenotype}

    "should calculate the expected number of offspring displaying dominant phenotype" in {
      val numberOfGenotypePairingCouples: List[Int] = getData(isPractice = true)
      calcNumberOfOffspringsDisplayingDominantGenotype(numberOfGenotypePairingCouples) shouldEqual
        (3.5 +- absoluteTolerance)
    }
  }

  "IndependentAlleles" - {
    import IndependentAlleles.{getData, probabilityThatKthGenerationHasAtLeastNAaBb}

    "should calculate the probability that at least n AaBb organism will belong to the kth generation" in {
      val List(k, n): List[Int] = getData(isPractice = true)
      probabilityThatKthGenerationHasAtLeastNAaBb(k, n) shouldBe (0.684 +- absoluteTolerance)
    }
  }

}
