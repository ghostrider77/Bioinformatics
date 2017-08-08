package utils

sealed trait Nucleotide
sealed trait DnaNucleotide extends Nucleotide
sealed trait RnaNucleotide extends Nucleotide

case object Adenine extends DnaNucleotide with RnaNucleotide
case object Cytosine extends DnaNucleotide with RnaNucleotide
case object Guanine extends DnaNucleotide with RnaNucleotide
case object Thymine extends DnaNucleotide
case object Uracil extends RnaNucleotide


object Nucleotide {

  def toChar(nucleotide: Nucleotide): Char = nucleotide match {
    case Adenine => 'A'
    case Cytosine => 'C'
    case Guanine => 'G'
    case Thymine => 'T'
    case Uracil => 'U'
  }
}

class InvalidNucleotideException(char: Char) extends Throwable {
  override def getMessage: String = s"character $char is not a valid nucleotide"
}
