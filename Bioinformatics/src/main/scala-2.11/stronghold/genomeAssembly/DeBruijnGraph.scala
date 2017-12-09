package stronghold.genomeAssembly


case class Node(label: String)

class DeBruijnGraph(kMers: Seq[String], k: Int) {

  lazy val adjacencyList: Map[Node, List[Node]] = createAdjacencyList(this.kMers)

  def createAdjacencyList(kMers: Seq[String]): Map[Node, List[Node]] =
    (for { kMer <- kMers } yield Node(kMer.take(this.k - 1)) -> Node(kMer.drop(1)))
      .groupBy(_._1)
      .mapValues(neighbours => neighbours.map(_._2).toList)

}

object DeBruijnGraph {
  type Edge = (Node, Node)
  type Path = List[Edge]

  def getEdges(adjacencyList: Map[Node, List[Node]]): Iterator[Edge] =
    for {
      (node, neighbours) <- adjacencyList.toIterator
      neighbour <- neighbours
    } yield (node, neighbour)

  def getIndicesOfConnectingEdges(path: Path, unvisitedEdges: Vector[Edge]): List[Int] = {
    val lastNodeInPath: Node = path.head._2
    (for {
      (edge, ix) <- unvisitedEdges.zipWithIndex
      if edge._1 == lastNodeInPath
    } yield ix).toList
  }

  def completePathToEulerianPath(path: Path, unvisitedEdges: Vector[Edge], completePaths: Set[Path]): Set[Path] = {
    val indicesOfConnectingEdges: List[Int] = getIndicesOfConnectingEdges(path, unvisitedEdges)
    if (indicesOfConnectingEdges.isEmpty)
      if (unvisitedEdges.isEmpty) completePaths + path.reverse else completePaths
    else {
      val extendedPaths: Set[Path] = indicesOfConnectingEdges.foldLeft((Set[Path](), Set[Edge]())){
        case ((paths, newEdges), ix) =>
          val nextEdgeInPath: Edge = unvisitedEdges(ix)
          if (newEdges.contains(nextEdgeInPath)) (paths, newEdges)
          else {
            val remainingEdges: Vector[Edge] = unvisitedEdges.take(ix) ++ unvisitedEdges.drop(ix + 1)
            val updatedPaths: Set[Path] =
              completePathToEulerianPath(nextEdgeInPath :: path, remainingEdges, completePaths)
            (paths ++ updatedPaths, newEdges + nextEdgeInPath)
          }
      }._1
      completePaths.union(extendedPaths)
    }
  }

  def findAllEulerianPaths(adjacencyList: Map[Node, List[Node]]): Set[Path] = {
    val graphEdges: Vector[Edge] = getEdges(adjacencyList).toVector
    completePathToEulerianPath(path = List(graphEdges.head), unvisitedEdges = graphEdges.tail, completePaths = Set())
  }

}
