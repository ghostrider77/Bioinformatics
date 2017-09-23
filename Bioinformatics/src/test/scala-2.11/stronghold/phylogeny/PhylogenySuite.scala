package stronghold.phylogeny

import org.scalatest.{FreeSpec, Matchers}

class PhylogenySuite extends FreeSpec with Matchers {

  "CountingPhylogeneticAncestors" - {
    import CountingPhylogeneticAncestors.{getData, numberOfInnerNodesInUnrootedBinaryTree}

    "should calculate the number of internal nodes of any unrooted binary tree having n leaves" in {
      val numberOfLeaves: Int = getData(isPractice = true)
      numberOfInnerNodesInUnrootedBinaryTree(numberOfLeaves) shouldEqual 2
    }
  }
}
