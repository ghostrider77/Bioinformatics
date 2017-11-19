package stronghold.graphs

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MutableMap}

case class TrieEdge(parentNodeId: Int, nodeId: Int, data: Char)

sealed abstract class TrieNode {
  val label: Int
  val children: MutableMap[Char, Node]

  def addChild(n: Int, letter: Char): Node = children.getOrElseUpdate(letter, Node(n, letter))
}

case object Root extends TrieNode {
  val label: Int = 1
  val children: MutableMap[Char, Node] = MutableMap()
}

case class Node(label: Int, data: Char) extends TrieNode {
  val children: MutableMap[Char, Node] = MutableMap()
}

class Trie() {
  val rootNode: TrieNode = Root
  private val labelGenerator: Iterator[Int] = Iterator.from(2)

  private def getChildContainingLetter(currentNode: TrieNode, letter: Char): Option[Node] =
    currentNode.children.get(letter)

  def addWord(word: String): Unit = {
    @tailrec
    def insert(chars: List[Char], currentNode: TrieNode): Unit = {
      if (chars.nonEmpty) {
        val letter: Char = chars.head
        getChildContainingLetter(currentNode, letter) match {
          case None =>
            val label: Int = labelGenerator.next()
            val nextNode: Node = currentNode.addChild(label, letter)
            insert(chars.tail, nextNode)
          case Some(childNode) => insert(chars.tail, childNode)
        }
      }
    }
    insert(word.toList, rootNode)
  }

}

object Trie {

  def apply(words: Seq[String]): Trie = {
    val trie = new Trie()
    for { word <- words } trie.addWord(word)
    trie
  }

  def traverseTrie(trie: Trie): Set[TrieEdge] = {
    @tailrec
    def loop(currentNodes: Set[TrieNode], edges: Set[TrieEdge]): Set[TrieEdge] = {
      if (currentNodes.isEmpty) edges
      else {
        val nextNodes: Set[TrieNode] = currentNodes.flatMap(_.children.values)
        val newEdges: Set[TrieEdge] = for {
          parentNode <- currentNodes
          child <- parentNode.children.valuesIterator
        } yield TrieEdge(parentNode.label, child.label, child.data)
        loop(nextNodes, newEdges ++ edges)
      }
    }

    loop(Set(trie.rootNode), Set())
  }

}
