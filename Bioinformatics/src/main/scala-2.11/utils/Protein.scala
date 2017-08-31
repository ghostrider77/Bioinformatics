package utils

case class Protein(sequence: List[AminoAcid]) {
  override def toString: String = sequence.map(AminoAcid.toChar).mkString("")
}

object Protein {
  def apply(sequence: String): Protein = Protein(sequence.map(AminoAcid(_)).toList)
}
