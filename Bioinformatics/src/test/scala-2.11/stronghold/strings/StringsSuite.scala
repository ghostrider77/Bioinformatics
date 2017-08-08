package stronghold.strings

import org.scalatest.{FreeSpec, Matchers}
import utils.{Dna, Rna}
import utils.UtilityFunctions.Fasta

class StringsSuite extends FreeSpec with Matchers {

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

}
