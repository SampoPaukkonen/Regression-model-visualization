package regression

import scala.collection.mutable.Buffer
import scala.math.min
import scala.math.pow

/*
 * Class representing the graph of polynomial regression.
 * @param name is the name of the graph
 * @param variableNames are the names of the dependent and independent variable
 * @param polynomial order is the highest polynomial order to be used in this graph.
 * Note that polynomial orders are interpret as n - 1, so order of 2 would be 1 (line).
 * @param originalData are the data points which this graph is supposed to model.
 */

class PolynomialGraph(
  val name:                 String,
  val variableNames:        (String, String),
  val polynomialOrder:      Int,
  private var originalData: Buffer[(Double, Double)]) extends GeneralGraph {

  private val operator = new RegressionMatrixCalculator(this.originalData, this.polynomialOrder)
  private val coefficientVector = operator.deliverCoefficients()
  this.originalData = this.originalData.sortBy(_._2)
  private var maximumX: Double = originalData.maxBy(_._2)._2
  private var minimumX: Double = originalData.minBy(_._2)._2
  /*
   * Data points are by default sorted by the x value
   */

  override def toString: String = {
    var body = "y = "
    body += s"\n${this.coefficientVector.get(0, 0)}"
    for (i <- 1 until this.coefficientVector.numRows()) {
      body += s"\n+ ${this.coefficientVector.get(i, 0)}*x^(${i}) "
    }
    body
  }

  /*
   * Plot points are calculated with x value from lowerBound to upperBound by the step size.
   */
  def calculatePlotPoints(step: Double, lowerBound: Double, upperBound: Double): Buffer[(Double, Double)] = {
    val graphPoints = Buffer[(Double, Double)]()
    for (i <- lowerBound to upperBound by step) {
      val x = i
      val y = this.estimateY(x)
      graphPoints.append((y, x))
    }
    graphPoints

  }

  def maximumXDataPoint: Double = this.maximumX

  def minimumXDataPoint: Double = this.minimumX

  /*
   * Y is estimated to be each coefficient times its corresponding value to the power of its index
   * This is because y is supposed to be
   * y = b + a_1 * x_1 + a_2 * (x_1)^(2) + ... + (x_1)^(n)
   */
  def estimateY(value: Double): Double = {
    var y = 0.0
    for (c <- 0 until coefficientVector.numRows()) {
      y = y + coefficientVector.get(c, 0).toDouble * pow(value, c)
    }
    y
  }

  def roundedMaximumXPoint(roundTo: Double = 1.0): Double = {
    var rounder = roundTo
    if (roundTo <= 0) rounder = 1
    scala.math.ceil(this.maximumX / rounder) * rounder
  }

  def dataPoints = this.originalData

  def dataPointsUntilMaxX(maximum: Double = this.maximumX): Buffer[(Double, Double)] = originalData.takeWhile(_._2 <= maximum)

  def setMaximumXDataPoint(newMax: Double): Unit = maximumX = newMax

  def setMinimumXDataPoint(newMin: Double): Unit = minimumX = newMin

  def giveVariableNames: (String, String) = this.variableNames
}