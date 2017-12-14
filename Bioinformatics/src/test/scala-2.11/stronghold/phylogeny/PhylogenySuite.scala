package stronghold.phylogeny

import org.scalatest.{FreeSpec, Matchers, Inspectors}

class PhylogenySuite extends FreeSpec with Matchers with Inspectors {

  object Constants {
    val absoluteTolerance: Double = 0.001
  }

  "CountingPhylogeneticAncestors" - {
    import CountingPhylogeneticAncestors.{getData, numberOfInnerNodesInUnrootedBinaryTree}

    "should calculate the number of internal nodes of any unrooted binary tree having n leaves" in {
      val numberOfLeaves: Int = getData(isPractice = true)
      numberOfInnerNodesInUnrootedBinaryTree(numberOfLeaves) shouldEqual 2
    }
  }

  "CreatingADistanceMatrix" - {
    import CreatingADistanceMatrix.{getData, calcDistanceMatrix}
    import utils.Dna
    import Constants.absoluteTolerance

    "Should retrieve the distance matrix corresponding to the p-distance on the given string" in {
      val dnas: List[Dna] = getData(isPractice = true)
      val expectedDistanceMatrix: Array[Array[Double]] =
        Array(
          Array(0.0, 0.4, 0.1, 0.1),
          Array(0.4, 0.0, 0.4, 0.3),
          Array(0.1, 0.4, 0.0, 0.2),
          Array(0.1, 0.3, 0.2, 0.0)
        )
      val calculatedDistanceMatrix: Array[Array[Double]] = calcDistanceMatrix(dnas)
      forAll(expectedDistanceMatrix.flatten.toList.zip(calculatedDistanceMatrix.flatten.toList)) {
        case (expected, calculated) => expected shouldBe (calculated +- absoluteTolerance)
      }
    }
  }

  "DistancesInTree" - {
    import DistancesInTrees.{NewickTree, NodePair, getData, calcDistancesInNewickTrees, calcDistanceBetweenNodes}

    "should calculate the distance between two nodes in an unweighted Newick tree for the test cases" in {
      val treesAndNodePairs: List[(NewickTree, NodePair)] = getData(isPractice = true)
      calcDistancesInNewickTrees(treesAndNodePairs) shouldEqual List(1, 2)
    }

    "should calculate the distance between two nodes in an unweighted Newick tree for a larger tree" in {
      val tree: NewickTree = "(((a,b),(c,d)x),((f,g),e)y);"
      val node1: String = "a"
      val node2: String = "e"
      calcDistanceBetweenNodes(tree, node1, node2) shouldEqual 5
    }
  }

  "CreatingACharacterTable" - {
    import CreatingACharacterTable.{getData, createCharacterTableFromNewickTree}

    "should create a character table having the same splits as the edge splits of the tree" in {
      val newickTreeString: String = getData(isPractice = true)
      createCharacterTableFromNewickTree(newickTreeString) should contain theSameElementsAs
        List(List(false, false, true, true, false), List(false, false, true, true, true))
    }
  }
}
