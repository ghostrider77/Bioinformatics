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

  "InferringProteinFromSpectrum" - {
    import InferringProteinFromSpectrum.{getData, getProteinFromPrefixSpectrum}

    "should return a protein string whose prefix spectrum is equal to the given list of masses" in {
      val prefixSpectrum: List[Double] = getData(isPractice = true)
      getProteinFromPrefixSpectrum(prefixSpectrum).toString shouldEqual "WMQS"
    }
  }

  "SpectralConvolution" - {
    import SpectralConvolution.{getData, calcConvolutionOfSpectra}
    import Constants.absoluteTolerance

    "should calculate the largest multiplicity of the convolution of two spectra" in {
      val List(spectrum1, spectrum2): List[List[Double]] = getData(isPractice = true)
      val (multiplicity, shift): (Int, Double) = calcConvolutionOfSpectra(spectrum1, spectrum2)
      multiplicity shouldEqual 3
      shift shouldBe (85.03163 +- absoluteTolerance)
    }
  }

}
