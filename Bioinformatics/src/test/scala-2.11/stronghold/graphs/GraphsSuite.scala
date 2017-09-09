package stronghold.graphs

import org.scalatest.{FreeSpec, Matchers}
import algorithms.Datastructures.Graph
import utils.GraphUtilityFunctions.Component


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

  "CompletingATree" - {
    import CompletingATree.getData

    "Should retrieve the minimum number of edges that can be added to the graph to produce a tree" in {
      val graph: Graph = getData(isPractice = true)
      val components: List[Component] = graph.connectedComponents.get
      components.length - 1 shouldEqual 3
    }
  }

}
