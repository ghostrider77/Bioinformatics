package utils

case class Codon(codon: List[RnaNucleotide]) {
    if (codon.length != 3) throw new InvalidCodonLengthException(codon)
}

class InvalidCodonLengthException(nucleotides: Seq[RnaNucleotide]) extends Throwable {
  override def getMessage: String = s"Tried to create codon from nucleotides $nucleotides with invalid length."
}
