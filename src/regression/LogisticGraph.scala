package regression

import scala.collection.mutable.Buffer
import scala.math.exp
/*
 * Class representing the graph of a logistic regression.
 * @param name is the name of the graph
 * @param variableNames are the names of the dependent and independent variable
 * @param originalData are the data points which this graph is supposed to model.
 */
class LogisticGraph(val name: String, val variableNames: (String, String), private var originalData: Buffer[(Double, Double)]) extends GeneralGraph {
  //operator is used internally to derive coefficients for this graph.
  private val operator    = LogisticRegressionCalculator
  /*
   * For deriving the coefficients the operator expects in separate arrays, sorted
   */
  private val temp        = originalData.sortBy(_._2).unzip
  private val xData       = temp._2.toArray
  private val yData       = temp._1.toArray
  private var maxX        = xData.max
  private var minX        = xData.min
  //All the values of the independent variable are scaled between 0 and 1.
  //This prevents potential overflow with the exponential function.
  private val scaledXData = xData.map(z => (z - this.minX) / (this.maxX - this.minX))
  private val thetas      = operator.deriveThetas(yData, scaledXData)
  //After scaling the maximum and minimum of the graph are 1 and 0 respectively
  private val scaledXMin: Double  = 0.0
  private val scaledXMax: Double  = 1.0
  
  
  override def toString: String = {
    var body = "Coefficients:"
    for (theta <- thetas.indices) {
      body += s"\ntheta${theta}: ${thetas(theta)}"
    }
    body += s"\n\nEquation:\nh_theta(X) = 1 / (1 + e^-(Theta^(T)*X))"
    body
  }
  
  
  def calculatePlotPoints(numberOfSteps: Double, lowerBound: Double, upperBound: Double): Buffer[(Double, Double)] = {
    var curvePoints = Buffer[(Double, Double)]()
    /*
     * Here numberOfSteps is interpret as dx.
     * The dx given by GUI is derived with the unscaled minimum and maximum x values.
     * In order to be used with the scaled x values the dx needs to be scaled.
     * (here ' denotes scaled value)
     * And it happens that if dx = (max - min) / w then w = (max - min) / dx.
     * And when max' = 1 and min' = 0, then
     * w = (max' - min') / dx' <-> w = (1 - 0) / dx' <-> dx' = 1 / w.
     * And so dx' = dx / (max - min).
     */
    
    val dx = numberOfSteps / (this.maxX - this.minX) 
    for (i <- scaledXMin to scaledXMax by dx) {
      val x = i
      val y = exp(this.thetas(0) + this.thetas(1) * i) / (1.0 + exp((this.thetas(0) + this.thetas(1) * i)))
      /*
       * Because the relation between depended and independed variable is unaffected by the value scaling, the scaled values
       * can be used to calculate the y value of the plot point.
       * At the end the scaled value is rescaled back to its original self for the plot.
       * For the user it's more meaningful to to see the independent variable with the original values rather than with the
       * scaled ones.
       */
      val reScaledX = x * (this.maxX - this.minX) + this.minX
      curvePoints.append((y, reScaledX))
    }
    curvePoints
  }
  
  def dataPointsUntilMaxX(maximum: Double = this.maxX): Buffer[(Double, Double)] = {
    originalData.takeWhile(_._2 <= maximum)
  }
  
  def maximumXDataPoint: Double = this.maxX //values are in tuples with the form of (y, x)
  
  def minimumXDataPoint: Double = this.minX 
  /*
   * Dependent variable value is estimated with the value parameter
   * function used is the form of e^(a) /( 1 + e^(a)) 
   */
  def estimateY(value: Double)  = {
    val scaledValue = (value - this.minX) / (this.maxX - this.minX)
    exp(this.thetas(0) + this.thetas(1) * scaledValue) / (1.0 + exp((this.thetas(0) + this.thetas(1) *scaledValue)))
    
  }
  
  def giveVariableNames: (String, String) = this.variableNames
  
  def setMaximumXDataPoint(newMax: Double): Unit = this.maxX = newMax
  
  def setMinimumXDataPoint(newMin: Double): Unit  = this.minX = newMin
  
  def roundedMaximumXPoint(roundTo: Double = 1.0): Double = {
    var rounder = roundTo
    if (roundTo <= 0) rounder = 1
    scala.math.ceil(this.maxX / rounder) * rounder
  }
  
  
  
  
}