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

  "ConstructingADeBruijnGraph" - {
    import ConstructingADeBruijnGraph.{getData, createDeBruijnGraph}
    import DeBruijnGraph.getEdges
    import utils.Dna

    "should construct the adjacency list of the DeBriujn-graph of the given kmers and their reverse complements" in {
      val dnaStrings: List[Dna] = getData(isPractice = true)
      val graph: DeBruijnGraph = createDeBruijnGraph(dnaStrings)
      val edges: List[(String, String)] =
        getEdges(graph.adjacencyList).map{ case (n1, n2) => (n1.label, n2.label)}.toList
      edges should contain theSameElementsAs
        List(
          ("ATC", "TCA"),
          ("ATG", "TGA"),
          ("ATG", "TGC"),
          ("CAT", "ATC"),
          ("CAT", "ATG"),
          ("GAT", "ATG"),
          ("GCA", "CAT"),
          ("TCA", "CAT"),
          ("TGA", "GAT")
        )
    }
  }

  "AssessingAssemblyQuality" - {
    import AssessingAssemblyQuality.{getData, calcNStatistics}
    import utils.Dna

    "should calculate the N50 and N75 statistics for a collection of DNA strings" in {
      val dnaStrings: List[Dna] = getData(isPractice = true)
      val n50: Int = calcNStatistics(dnaStrings, 50)
      val n75: Int = calcNStatistics(dnaStrings, 75)
      n50 shouldEqual 7
      n75 shouldEqual 6
    }
  }

}
