package utils

import java.io.{BufferedWriter, File, FileWriter, InputStream}
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import Rna.toNucleotide

object UtilityFunctions {

  case class Fasta(name: String, string: String)

  case class Alignment(alignedString1: String, alignedString2: String, alignmentScore: Int)

  def convertStringToIntList(line: String, sep: String = " "): List[Int] = line.split(sep).map(_.toInt).toList

  def convertToOneBasedIndexing(indices: List[Int]): List[Int] = indices.map(_ + 1)

  def readInputData(filename: String): List[String] = {
    val fileStream: InputStream = getClass.getResourceAsStream(filename)
    val lines: Iterator[String] = scala.io.Source.fromInputStream(fileStream).getLines()
    lines.toList
  }

  def readFastaSequences(lines: List[String], isLineSeparated: Boolean = true): List[Fasta] = {
    if (isLineSeparated) (for { List(name, sequence) <- lines.sliding(2, 2) } yield Fasta(name.tail, sequence)).toList
    else {
      val sequences: ListBuffer[Fasta] = ListBuffer()

      @tailrec
      def loop(ls: List[String], currentName: String, currentSequence: StringBuilder): Unit = ls match {
        case Nil => sequences += Fasta(currentName, currentSequence.toString)
        case line :: lss =>
          if (line.startsWith(">")) {
            sequences += Fasta(currentName, currentSequence.toString)
            loop(lss, line.tail, new StringBuilder())
          }
          else loop(lss, currentName, currentSequence ++= line)
      }

      loop(lines.tail, lines.head.tail, new StringBuilder())
      sequences.toList
    }
  }

  def readRnaCodonTable(): Map[Codon, Option[AminoAcid]] = {
    val filename: String = "/datafiles/RNA_codon_table.txt"
    val fileStream: InputStream = getClass.getResourceAsStream(filename)
    val lines: Iterator[String] = scala.io.Source.fromInputStream(fileStream).getLines()

    def aminoAcidEncodedByCodon(line: String): (Codon, Option[AminoAcid]) = line.split(" ").toList match {
      case List(codon, oneLetterCode) => (Codon(codon.map(toNucleotide).toList), Some(AminoAcid(oneLetterCode.head)))
      case List(codon) => (Codon(codon.map(toNucleotide).toList), None)
    }

    lines.map(aminoAcidEncodedByCodon).toMap
  }

  def writeListAsStringToFile[T](lst: List[T], outputFilename: String = "output.txt", sep: String = " "): Unit = {
    val file = new File(outputFilename)
    val bw = new BufferedWriter(new FileWriter(file))
    val result: String = lst.mkString(sep)
    bw.write(result)
    bw.close()
  }

  def writeListOfListsAsStringsToFile[T](lists: List[List[T]],
                                         outputFilename: String = "output.txt",
                                         sep: String = " "): Unit = {
    val file = new File(outputFilename)
    val bw = new BufferedWriter(new FileWriter(file))
    for { lst <- lists } {
      val line: String = lst.mkString(sep)
      bw.write(line)
      bw.write("\n")
    }
    bw.close()
  }

  def writeNucleotideListToFileAsString(nucleotides: List[Nucleotide],
                                        outputFilename: String = "output.txt"): Unit = {
    val file = new File(outputFilename)
    val bw = new BufferedWriter(new FileWriter(file))
    val result: String = nucleotides.map(Nucleotide.toChar).mkString("")
    bw.write(result)
    bw.close()
  }

  def writeProteinToFileAsString(protein: Protein, outputFilename: String = "output.txt"): Unit = {
    val file = new File(outputFilename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(protein.toString)
    bw.close()
  }

  def writeAlignmentToFile(alignment: Alignment, outputFilename: String = "output.txt"): Unit = {
    writeListOfListsAsStringsToFile(
      List(
        List(alignment.alignmentScore.toString),
        List(alignment.alignedString1),
        List(alignment.alignedString2)
      ),
      outputFilename = outputFilename
    )
  }

}
