package utils

import scala.annotation.tailrec
import Nucleotide.toChar

case class Dna(sequence: List[DnaNucleotide]) {
  lazy val length: Int = sequence.length
  val complements: Map[DnaNucleotide, DnaNucleotide] =
    Map(
      Adenine -> Thymine,
      Cytosine -> Guanine,
      Guanine -> Cytosine,
      Thymine -> Adenine
    )

  override def toString: String = sequence.map(toChar).mkString("")

  def reverseComplement: Dna = {
    @tailrec
    def loop(ns: List[DnaNucleotide], acc: List[DnaNucleotide]): List[DnaNucleotide] = ns match {
      case Nil => acc
      case nucleotide :: nss => loop(nss, complements(nucleotide) :: acc)
    }
    Dna(loop(this.sequence, Nil))
  }
}

object Dna {

  def apply(sequence: String): Dna = Dna(sequence.toList.map(toNucleotide))

  def toNucleotide(char: Char): DnaNucleotide = char match {
    case 'A' => Adenine
    case 'C' => Cytosine
    case 'G' => Guanine
    case 'T' => Thymine
    case _ => throw new InvalidNucleotideException(char)
  }

  def transcribe(dna: Dna): Rna = Rna(dna.sequence.map(transcribeNucleotide))

  def transcribeNucleotide(nucleotide: DnaNucleotide): RnaNucleotide = nucleotide match {
    case Adenine => Adenine
    case Cytosine => Cytosine
    case Guanine => Guanine
    case Thymine => Uracil
  }

  def convertDNAMapToList[T](map: Map[DnaNucleotide, T])(implicit num: Numeric[T]): List[T] = {
    List(
      map.getOrElse(Adenine, num.zero),
      map.getOrElse(Cytosine, num.zero),
      map.getOrElse(Guanine, num.zero),
      map.getOrElse(Thymine, num.zero)
    )
  }
}
