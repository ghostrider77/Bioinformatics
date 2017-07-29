package utils

import scala.annotation.tailrec

sealed trait BaseSequence

case class Dna(sequence: List[DnaNucleotide]) extends BaseSequence {
  lazy val length: Int = sequence.length
  val complements: Map[DnaNucleotide, DnaNucleotide] = Map(A -> T, C -> G, G -> C, T -> A)

  override def toString: String = sequence.mkString("")

  def reverseComplement: Dna = {
    @tailrec
    def loop(ns: List[DnaNucleotide], acc: List[DnaNucleotide]): List[DnaNucleotide] = ns match {
      case Nil => acc
      case nucleotide :: nss => loop(nss, complements(nucleotide) :: acc)
    }
    Dna(loop(this.sequence, Nil))
  }
}

case class Rna(sequence: List[RnaNucleotide]) extends BaseSequence {
  lazy val length: Int = sequence.length

  override def toString: String = sequence.mkString("")
}

object Dna {

  def apply(sequence: String): Dna = Dna(sequence.toList.map(toNucleotide))

  def toNucleotide(char: Char): DnaNucleotide = char match {
    case 'A' => A
    case 'C' => C
    case 'G' => G
    case 'T' => T
    case _ => throw new InvalidNucleotideException(char)
  }

  def transcribe(dna: Dna): Rna = Rna(dna.sequence.map(transcribeNucleotide))

  def transcribeNucleotide(nucleotide: DnaNucleotide): RnaNucleotide = nucleotide match {
    case A => A
    case C => C
    case G => G
    case T => U
  }

  def convertDNAMapToList[T](map: Map[DnaNucleotide, T])(implicit num: Numeric[T]): List[T] = {
    List(
      map.getOrElse(A, num.zero),
      map.getOrElse(C, num.zero),
      map.getOrElse(G, num.zero),
      map.getOrElse(T, num.zero)
    )
  }
}
