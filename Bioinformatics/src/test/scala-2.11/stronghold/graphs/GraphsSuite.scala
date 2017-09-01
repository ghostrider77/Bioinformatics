package stronghold.graphs

import org.scalatest.{FreeSpec, Matchers}


class GraphsSuite extends FreeSpec with Matchers {

  "OverlapGraphs" - {
    import OverlapGraphs.{getData, createOverlapGraph}
    import utils.UtilityFunctions.Fasta

    "should calculate the overlap graph of DNA strings" in {
      val dnaStrings: List[Fasta] = getData(isPractice = true)
      createOverlapGraph(dnaStrings, k = 3) shouldEqual
        Map("Rosalind_0498" -> List("Rosalind_2391", "Rosalind_0442"), "Rosalind_2391" -> List("Rosalind_2323"))
    }
  }

}
