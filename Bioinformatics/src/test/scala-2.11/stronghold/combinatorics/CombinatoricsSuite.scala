package stronghold.combinatorics

import org.scalatest.{FreeSpec, Matchers}
import utils.UtilityFunctions.readRnaCodonTable
import utils.{AminoAcid, Codon, Protein}


class CombinatoricsSuite extends FreeSpec with Matchers {
  private lazy val codonTable: Map[Codon, Option[AminoAcid]] = readRnaCodonTable()

  "InferingMRnaFromProtein" - {
    import InferingMRnaFromProtein.{getData, calcNumberOfRnaStringsTranslateIntoProtein}

    "should calculate the number of RNA strings from which a protein can be derived" in {
      val protein: Protein = getData(isPractice = true)
      calcNumberOfRnaStringsTranslateIntoProtein(protein, codonTable) shouldEqual 12
    }

  }

  "RabbitsAndRecurrence" - {
    import RabbitsAndRecurrence.{getData, calcGeneralizedFibonacciSequence}

    "should calculate the total number of rabbit pairs after n months" in {
      val List(n, k): List[Int] = getData(isPractice = true)
      calcGeneralizedFibonacciSequence(n, k) shouldEqual 19L
    }

    "should have 1 + k rabbit pairs in the 3rd month" in {
      val numberOfMonths: Int = 3
      val reproductionOfARabbitPair: Int = 10
      calcGeneralizedFibonacciSequence(numberOfMonths, reproductionOfARabbitPair) shouldEqual
        1 + reproductionOfARabbitPair
    }
  }

}
