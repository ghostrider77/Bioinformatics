package utils

sealed trait AminoAcid

case object Alanine extends AminoAcid
case object Arginine extends AminoAcid
case object Asparagine extends AminoAcid
case object AsparticAcid extends AminoAcid
case object Cysteine extends AminoAcid
case object GlutamicAcid extends AminoAcid
case object Glutamine extends AminoAcid
case object Glycine extends AminoAcid
case object Histidine extends AminoAcid
case object Isoleucine extends AminoAcid
case object Leucine extends AminoAcid
case object Lysine extends AminoAcid
case object Methionine extends AminoAcid
case object Phenylalanine extends AminoAcid
case object Proline extends AminoAcid
case object Serine extends AminoAcid
case object Threonine extends AminoAcid
case object Tryptophan extends AminoAcid
case object Tyrosine extends AminoAcid
case object Valine extends AminoAcid


object AminoAcid {
  def apply(char: Char): AminoAcid = char match {
    case 'A' => Alanine
    case 'R' => Arginine
    case 'N' => Asparagine
    case 'D' => AsparticAcid
    case 'C' => Cysteine
    case 'E' => GlutamicAcid
    case 'Q' => Glutamine
    case 'G' => Glycine
    case 'H' => Histidine
    case 'I' => Isoleucine
    case 'L' => Leucine
    case 'K' => Lysine
    case 'M' => Methionine
    case 'F' => Phenylalanine
    case 'P' => Proline
    case 'S' => Serine
    case 'T' => Threonine
    case 'W' => Tryptophan
    case 'Y' => Tyrosine
    case 'V' => Valine
  }

  def toChar(aminoAcid: AminoAcid): Char = aminoAcid match {
    case Alanine => 'A'
    case Arginine => 'R'
    case Asparagine => 'N'
    case AsparticAcid => 'D'
    case Cysteine => 'C'
    case GlutamicAcid => 'E'
    case Glutamine => 'Q'
    case Glycine => 'G'
    case Histidine => 'H'
    case Isoleucine => 'I'
    case Leucine => 'L'
    case Lysine => 'K'
    case Methionine => 'M'
    case Phenylalanine => 'F'
    case Proline => 'P'
    case Serine => 'S'
    case Threonine => 'T'
    case Tryptophan => 'W'
    case Tyrosine => 'Y'
    case Valine => 'V'
  }

}
