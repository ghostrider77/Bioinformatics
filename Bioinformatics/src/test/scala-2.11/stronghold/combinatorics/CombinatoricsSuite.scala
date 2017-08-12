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

}
