package regression

import scala.collection.mutable.Buffer
/*
 * GeneralGraph represents all graphs and their common traits. 
 * GUI is implemented so that any Graph which inherits the GeneralGraph works in it.
 */
trait GeneralGraph {
  def calculatePlotPoints(step: Double, lowerBound: Double, upperBound: Double): Buffer[(Double, Double)]
  def dataPointsUntilMaxX(maximum: Double): Buffer[(Double, Double)]
  def name: String
  def giveVariableNames: (String, String)
  
  def maximumXDataPoint: Double
  def minimumXDataPoint: Double
  def setMaximumXDataPoint(newMax: Double): Unit
  def setMinimumXDataPoint(newMin: Double): Unit
  /*
   * @param value is the value which is supposed to be estimated by the graph's regression model.
   */
  def estimateY(value: Double): Double
  /*
   * @param roundToThis is the value to which the maximum x point of the graph is rounded to.
   */
  def roundedMaximumXPoint(roundToThis: Double): Double
  
  
  
}
