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

  "EnumeratingGeneOrders" - {
    import EnumeratingGeneOrders.{getData, getPermutations}

    "should calculate all permutations of the number 1 to n" in {
      val n: Int = getData(isPractice = true)
      val perms: List[List[Int]] = getPermutations(n)
      perms.length shouldEqual 6
      perms.toSet shouldEqual
        Set(List(1, 2, 3), List(1, 3, 2), List(2, 1, 3), List(2, 3, 1), List(3, 1, 2), List(3, 2, 1))
    }
  }

  "EnumeratingKmersLexicographically" - {
    import EnumeratingKmersLexicographically.{getData, getOrderedKmers}

    "should create all k-mers for the sample problem" in {
      val (alphabet, k): (List[String], Int) = getData(isPractice = true)
      getOrderedKmers(alphabet, k) shouldEqual
        List("AA", "AC", "AG", "AT", "CA", "CC", "CG", "CT", "GA", "GC", "GG", "GT", "TA", "TC", "TG", "TT")
    }

    "should create all k-mers in the given lexicographic order" in {
      val alphabet: List[String] = List("B", "A", "C")
      getOrderedKmers(alphabet, 1) shouldEqual List("B", "A", "C")
      getOrderedKmers(alphabet, 2) shouldEqual List("BB", "BA", "BC", "AB", "AA", "AC", "CB", "CA", "CC")
    }
  }

  "OrderingStringsOfVaryingLength" - {
    import OrderingStringsOfVaryingLength.{getData, generateOrderedAtMostKmers}

    "should create all strings from the given alphabet with length at most k for the sample problem" in {
      val (alphabet, k): (List[String], Int) = getData(isPractice = true)
      generateOrderedAtMostKmers(alphabet, k) shouldEqual
        List(
          "D", "DD", "DDD", "DDN", "DDA", "DN", "DND", "DNN", "DNA", "DA", "DAD", "DAN", "DAA",
          "N", "ND", "NDD", "NDN", "NDA", "NN", "NND", "NNN", "NNA", "NA", "NAD", "NAN", "NAA",
          "A", "AD", "ADD", "ADN", "ADA", "AN", "AND", "ANN", "ANA", "AA", "AAD", "AAN", "AAA"
        )
    }
  }

  "MortalFibonacciRabbits" - {
    import MortalFibonacciRabbits.{getData, calcMortalFibonacciSequence}

    "should calculate the total number of rabbit pairs after n months when each rabbit lives for m months" in {
      val List(n, m): List[Int] = getData(isPractice = true)
      calcMortalFibonacciSequence(n, m) shouldEqual 4
    }
  }

}
