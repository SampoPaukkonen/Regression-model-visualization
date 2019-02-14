package regression

import scala.collection.mutable.Buffer
import java.io.File



/*
 * DataCollector's main purpose is to recognize the file type it's given and 
 * choose the correct method from Parser to read the file
 */
object DataCollector {

  /*
   * @param file is the file which is supposed to be read
   * Method first checks if the file ending matches either csv or xml. If not then exception is thrown.
   * Otherwise a proper method for the file is chosen.
   */
  def chooseTypeAndExtractFromFile(file: File) = {
    if (file.getName.takeRight(3).toString().toLowerCase() == "csv") {
      extractDataFromCSVFile(file)
    } else if (file.getName.takeRight(3).toString().toLowerCase == "xml") {
      extractDataFromXMLFile(file)
    } else {
      throw new InvalidFileData("File extension did not equal CSV or XML.")
    }
  }
  /*
   * extractDataFromCSVFile and extractDataFromXMLFile work identically in DataCollector but they are kept as separate methods for clarity.
   * The first value in the Parser's return value is assumed to be the names of the variables. 
   * Values are converted to Double from String in a try-catch structure. If a non-numeral is found then exception is raised.
   */
  def extractDataFromCSVFile(file: File): (Buffer[(Double, Double)], (String, String)) = {
    var rawDataValues = Buffer[(Double, Double)]()
    var names: (String, String) = ("", "")
    val parsedData = Parser.readFromCSVFile(file)
    names = parsedData(0) //Despite what the first tuple will include it will be used to name the axes
    rawDataValues = for (line <- parsedData.drop(1)) yield {
      try {
        (line._1.toDouble, line._2.toDouble)
      } catch {
        case e: java.lang.NumberFormatException => {
          println(line)
          throw new InvalidFileData(s"Non-numeric values were found in data points.")
        }
      }
    }
    (rawDataValues, names)
  }

  def extractDataFromXMLFile(file: File): (Buffer[(Double, Double)], (String, String)) = {
    var rawDataValues = Buffer[(Double, Double)]()
    var names: (String, String) = ("", "")
    val parsedData = Parser.readFromXMLFile(file)
    names = parsedData(0)
    rawDataValues = for (line <- parsedData.drop(1)) yield {
      try {
        (line._1.toDouble, line._2.toDouble)
      } catch {
        case e: java.lang.NumberFormatException => {
          throw new InvalidFileData(s"Non-numeric values were found in data points.")
        }
      }
    }
    (rawDataValues, names)
  }

}