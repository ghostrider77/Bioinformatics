package stronghold.massSpectrometry

import org.scalatest.{FreeSpec, Matchers}
import utils.UtilityFunctions.readMonoisotopicMassTable
import utils.{AminoAcid, Protein}


class MassSpectrometrySuite extends FreeSpec with Matchers {
  private lazy val aminoAcidMassTable: Map[AminoAcid, Double] = readMonoisotopicMassTable()

  object Constants {
    val absoluteTolerance: Double = 1e-03
  }

  "CalculatingProteinMass" - {
    import CalculatingProteinMass.{getData, calcProteinWeight}
    import Constants.absoluteTolerance

    "should calculate the mass of a protein" in {
      val protein: Protein = getData(isPractice = true)
      calcProteinWeight(protein, aminoAcidMassTable) shouldBe (821.392 +- absoluteTolerance)
    }
  }

}
