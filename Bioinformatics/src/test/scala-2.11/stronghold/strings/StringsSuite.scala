package stronghold.strings

import org.scalatest.{FreeSpec, Matchers}
import utils.{AminoAcid, Codon, Dna, Protein, Rna}
import utils.UtilityFunctions.{Fasta, readRnaCodonTable}

class StringsSuite extends FreeSpec with Matchers {
  private lazy val codonTable: Map[Codon, Option[AminoAcid]] = readRnaCodonTable()

  object Constants {
    val absoluteTolerance: Double = 0.001
  }

  "CountingDnaNucleotides" - {
    import CountingDnaNucleotides.{getData, countNucleotides}
    import utils.Dna.convertDNAMapToList

    "should count the number DNA nucleotides present in a given DNA string" in {
      val dna: Dna = getData(isPractice = true)
      convertDNAMapToList(countNucleotides(dna)) shouldEqual List(20, 12, 17, 21)
    }

    "should count the number of DNA nucleotides when not all of them are present" in {
      val dna: Dna = Dna(sequence = "ATTCCC")
      convertDNAMapToList(countNucleotides(dna)) shouldEqual List(1, 3, 0, 2)
    }
  }

  "TranscribingDnaIntoRna" - {
    import TranscribingDnaIntoRna.getData
    import utils.Dna.transcribe

    "should transcribe the sample DNA into RNA" in {
      val dna: Dna = getData(isPractice = true)
      val rna: Rna = transcribe(dna)
      rna.toString shouldEqual "GAUGGAACUUGACUACGUAAAUU"
    }
  }

  "ComplementingDnaStrand" - {
    import ComplementingDnaStrand.getData

    "should complement the dample DNA strand" in {
      val dna: Dna = getData(isPractice = true)
      dna.reverseComplement.toString shouldEqual "ACCGGGTTTT"
    }
  }

  "ComputingGcContent" - {
    import ComputingGcContent.{getData, findDnaWithHighestGcPercentage}
    import Constants.absoluteTolerance

    "should find the DNA sequence that has the highest GC-percentage" in {
      val sequences: List[Fasta] = getData(isPractice = true)
      val (result, gcContent): (Fasta, Double) = findDnaWithHighestGcPercentage(sequences)
      result.name shouldEqual "Rosalind_0808"
      (100 * gcContent) shouldBe (60.919540 +- absoluteTolerance)
    }
  }

  "FindingAMotifInDna" - {
    import FindingAMotifInDna.{getData, findStartingPositionsOfMotif}
    import utils.UtilityFunctions.convertToOneBasedIndexing

    "should find the starting motif positions for the sample problem" in {
      val List(dna, motif): List[Dna] = getData(isPractice = true)
      convertToOneBasedIndexing(findStartingPositionsOfMotif(dna, motif)) shouldEqual List(2, 4, 10)
    }

    "should find the starting positions of a motif correctly" in {
      val dna: Dna = Dna("AAAA")
      val motif: Dna = Dna("AAA")
      convertToOneBasedIndexing(findStartingPositionsOfMotif(dna, motif)) shouldEqual List(1, 2)
    }

    "should return not starting positions if no motif has been found" in {
      val dna: Dna = Dna("ATCGC")
      val motif: Dna = Dna("CGT")
      convertToOneBasedIndexing(findStartingPositionsOfMotif(dna, motif)) shouldBe empty
    }
  }

  "TranslatingRnaIntoProtein" - {
    import TranslatingRnaIntoProtein.{getData, translate}
    import utils.UtilityFunctions.readRnaCodonTable

    "should translate an RNA string into a protein string" in {
      val rna: Rna = getData(isPractice = true)
      val codonTable: Map[Codon, Option[AminoAcid]] = readRnaCodonTable()
      translate(rna, codonTable).toString shouldEqual "MAMAPRTEINSTRING"
    }
  }

  "FindingASharedMotif" - {
    import FindingASharedMotif.{getData, calcLongestCommonSubstring}

    "should find the longest shared substring in a collection of strings" in {
      val strings: List[String] = getData(isPractice = true)
      List("AC", "CA", "TA") should contain (calcLongestCommonSubstring(strings).get)
    }

    "should return an empty string if there is no common substring" in {
      val strings: List[String] = List("ACA", "GTT")
      calcLongestCommonSubstring(strings) shouldBe None
    }
  }

  "FindingASharedSplicedMotif" - {
    import FindingASharedSplicedMotif.{getData, calcLongestCommonSubsequence}

    "should find the longest common subsequence of two strings" in {
      val List(string1, string2): List[String] = getData(isPractice = true)
      List("AACTGG", "AACTTG") should contain (calcLongestCommonSubsequence(string1, string2))
    }

    "should return an empty string when there is no common subsequence" in {
      val string1: String = "ACAA"
      val string2: String = "TTTTTGGGGG"
      calcLongestCommonSubsequence(string1, string2) shouldBe empty
    }
  }

  "LocatingRestrictionSites" - {
    import LocatingRestrictionSites.{getData, findReversePalindromes}

    "should locate position and length of those substrings in a DNA that are palindromes" in {
      val List(sequence): List[Fasta] = getData(isPractice = true)
      val dna: Dna = Dna(sequence.string)
      findReversePalindromes(dna).map{ case (ix, l) => (ix + 1, l) }.toSet shouldEqual
        Set((4, 6), (5, 4), (6, 6), (7, 4), (17, 4), (18, 4), (20, 6), (21, 4))
    }
  }

  "ConsensusAndProfile" - {
    import ConsensusAndProfile.{getData, calcProfileAndConsensus, transformOutput}

    "should calculate the profile matrix of DNA strings and the consensus string" in {
      val sequences: List[Dna] = getData(isPractice = true)
      val (profile, consensus): (List[List[Int]], Dna) = calcProfileAndConsensus(sequences)
      val printableProfile: List[List[String]] = transformOutput(profile)

      consensus shouldEqual Dna("ATGCAACT")
      printableProfile shouldEqual
        List(
          List("A: 5 1 0 0 5 5 0 0"),
          List("C: 0 0 1 4 2 0 6 1"),
          List("G: 1 1 6 3 0 1 0 0"),
          List("T: 1 5 0 0 0 1 1 6")
        )
    }
  }

  "RnaSplicing" - {
    import RnaSplicing.{getData, findAndTranslateExons}

    "should identify the exons and translate it to a protein" in {
      val (dna, introns): (Dna, Set[Dna]) = getData(isPractice = true)
      val protein: Option[Protein] = findAndTranslateExons(dna, introns, codonTable)
      protein.get.toString shouldEqual "MVYIADKQHVASREAYGHMFKVCA"
    }
  }

}
