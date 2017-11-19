package stronghold.graphs

import org.scalatest.{FreeSpec, Matchers}
import algorithms.Datastructures.Graph
import utils.GraphUtilityFunctions.Component
import utils.UtilityFunctions.Fasta


class GraphsSuite extends FreeSpec with Matchers {

  "OverlapGraphs" - {
    import OverlapGraphs.{getData, createOverlapGraph}

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

  "GenomeAssemblyShortestSuperstring" - {
    import GenomeAssemblyShortestSuperstring.{getData, findShortestSuperstring}

    "should retrieve the shortest superstring containing all the given strings" in {
      val dnaStrings: List[Fasta] = getData(isPractice = true)
      findShortestSuperstring(dnaStrings) shouldEqual "ATTAGACCTGCCGGAATAC"
    }

    "should find the shortest superstring" in {
      val dnaStrings: List[Fasta] = List(Fasta("s1", "ABCBC"), Fasta("s2", "BCBCE"))
      findShortestSuperstring(dnaStrings) shouldEqual "ABCBCE"
    }
  }

  "PatternMatching" - {
    import PatternMatching.{getData, createTrieAdjacencyList}

    "should return the adjacency list corresponding to the trie built for given words" in {
      val strings: List[String] = getData(isPractice = true)
      val trie: Trie = Trie(strings)
      createTrieAdjacencyList(trie) should contain theSameElementsAs
        List("1 2 A", "2 3 T", "3 4 A", "4 5 G", "5 6 A", "3 7 C", "1 8 G", "8 9 A", "9 10 T")
    }
  }

}
