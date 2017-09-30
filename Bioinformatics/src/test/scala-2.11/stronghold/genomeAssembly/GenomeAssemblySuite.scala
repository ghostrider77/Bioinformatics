package stronghold.genomeAssembly

import org.scalatest.{FreeSpec, Matchers}

class GenomeAssemblySuite extends FreeSpec with Matchers {

  "ErrorCorrectionInReads" - {
    import ErrorCorrectionInReads.{getData, identifyIncorrectReads}
    import utils.Dna

    "should retrieve a list of all incorrectly sequenced reads and their corrections" in {
      val reads: List[Dna] = getData(isPractice = true)
      identifyIncorrectReads(reads).toSet shouldEqual
        Set((Dna("TTCAT"), Dna("TTGAT")), (Dna("GAGGA"), Dna("GATGA")), (Dna("TTTCC"), Dna("TTTCA")))
    }
  }

}
