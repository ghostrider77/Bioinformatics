package stronghold.strings

import org.scalatest.{FreeSpec, Matchers}

class StringsSuite extends FreeSpec with Matchers {
  import utils.{Dna, Rna}

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
    val dna: Dna = getData(isPractice = true)
    dna.reverseComplement.toString shouldEqual "ACCGGGTTTT"
  }

}
