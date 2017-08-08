package utils

case class Protein(sequence: List[AminoAcid]) {
  override def toString: String = sequence.map(AminoAcid.toChar).mkString("")
}
