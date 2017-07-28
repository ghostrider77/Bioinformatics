package utils


sealed trait Nucleotide

sealed trait DnaNucleotide extends Nucleotide

sealed trait RnaNucleotide extends Nucleotide

case object A extends Nucleotide with DnaNucleotide with RnaNucleotide
case object C extends Nucleotide with DnaNucleotide with RnaNucleotide
case object G extends Nucleotide with DnaNucleotide with RnaNucleotide
case object T extends Nucleotide with DnaNucleotide
case object U extends Nucleotide                    with RnaNucleotide

object Nucleotide {

  def toChar(nucleotide: Nucleotide): Char = nucleotide match {
    case A => 'A'
    case C => 'C'
    case G => 'G'
    case T => 'T'
    case U => 'U'
  }
}

class InvalidNucleotideException(char: Char) extends Throwable {
  override def getMessage: String = s"character $char is not a valid nucleotide"
}
