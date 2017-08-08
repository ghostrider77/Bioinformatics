package utils

import Nucleotide.toChar

case class Rna(sequence: List[RnaNucleotide]) {
  lazy val length: Int = sequence.length

  override def toString: String = sequence.map(toChar).mkString("")
}

object Rna {

  def apply(sequence: String): Rna = Rna(sequence.toList.map(toNucleotide))

  def toNucleotide(char: Char): RnaNucleotide = char match {
    case 'A' => Adenine
    case 'C' => Cytosine
    case 'G' => Guanine
    case 'U' => Uracil
    case _ => throw new InvalidNucleotideException(char)
  }

}
