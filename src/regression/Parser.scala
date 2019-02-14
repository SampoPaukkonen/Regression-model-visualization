package regression

import scala.io.Source
import java.io.File
import scala.collection.mutable.Buffer
import scala.xml.XML
import java.nio.charset.CodingErrorAction
import scala.io.Codec

/*
 * Object used for parsing of a given file.
 */

object Parser {
  /*
   * With coded most, if not all, types of CSV are supported.
   */
  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
  codec.onMalformedInput(CodingErrorAction.IGNORE)
  /*
   * In CSV files semicolon ; is used instead of comma
   * @param file is the file to be parsed i.e. file from which
   * variable names and data points are to be extracted.
   */
  def readFromCSVFile(file: File): Buffer[(String, String)] = {
    val source = Source.fromFile(file)
    val holder = Buffer[(String, String)]()
    for (line <- source.getLines()) {
      val currentLine = line.split(";")
      //Two checks below complement each other by making it extra sure that the currentLine is not inconsistent
      if (currentLine.size < 2) throw new InvalidFileData(s"Line has inconsistent values meaning that either one of both are missing.")
      if (currentLine(0).isEmpty() || currentLine(1).isEmpty) {
        throw new InvalidFileData(s"Line has inconsistent values meaning that either one of both are missing.")
      }
      holder.append((currentLine(0), currentLine(1)))
    }
    //If holder is less than two it means that either variable names of data points are missing from the file
    if (holder.size < 2) throw new InvalidFileData(s"Too little data detected.\nEither variable names, data or both were missing.")
    source.close()
    holder
  }
  /*
   * readFromXMLFile does virtually the same thing than readFrom CSVFile.
   * Main exception is that it makes sure the structure of the XML file is valid meaning that
   * certain tags are to be found from it.
   */
  def readFromXMLFile(file: File): Buffer[(String, String)] = {
    try {
      val xmlFile = XML.loadFile(file)
      //If no main tag is found and exception is raised.
      val main = xmlFile.label
      if (main.size < 2) throw new InvalidFileData(s"XML file didn't contain proper main label.")
      val variableNames = xmlFile.\("variableNames").\("name")
      val x = variableNames.\@("x")
      val y = variableNames.\@("y")
      if (x.isEmpty() || y.isEmpty()) throw new InvalidFileData(s"One or more variable name is missing.")
      val holder = Buffer((y, x))
      val values = xmlFile.\("values")
      if (values.isEmpty) throw new InvalidFileData(s"No datapoints were found.")
      for (value <- values.\\("value")) {
        if (value.attributes.size < 2) throw new InvalidFileData(s"Line has inconsistent values meaning that either one of both are missing.")
        holder.append((value.\@("y"), value.\@("x")))
      }
      holder
    } catch {
      //exceptions below are due to error in the XML file. Most often this happens when a closing or starting tag is missing.
      case security: java.security.PrivilegedActionException => {
        throw new InvalidFileData(s"Invalid structure detected.\nXML document structures must start and end within the same entity.")
      }
      case parsing: org.xml.sax.SAXParseException => {
        throw new InvalidFileData(s"Invalid structure detected.\nXML document structures must start and end within the same entity.")
      }
    }

  }

}