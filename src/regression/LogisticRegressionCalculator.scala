package regression

import scala.collection.mutable.Buffer
import scala.math.exp
import scala.math.abs
import scala.math.log
import scala.math.ceil

/*
 * Object used to derive coefficients for the logistic regression.
 */
object LogisticRegressionCalculator {

  //Learning rate
  private var alpha = 0.001
  //Tolerance
  private var epsilon = 0.000000000015
  //Number of iterations at most
  private var maxIterations = 1000000
  
  /*
   * @param yData is assumed to be the dependent variables data
   * @paramt xData is assumed to be the independent variables sorted data
   */
  def deriveThetas(yData: Array[Double], xData: Array[Double]): Buffer[Double] = {
    require(yData.size == xData.size)
    //temp variables are used for simultaneous updating 
    var theta0     = 0.0
    var tempTheta0 = theta0
    var theta1     = 0.0
    var tempTheta1 = theta1
    val dataSize   = yData.size
    val maxX       = xData.max
    val minX       = xData.min

    //Hypothesis function for logistic regression in the form of sigmoid function
    def hypothesis(z: Double) = exp(z) / (1.0 + exp(z))
    
    //Returns the value to be added or subtacted from the theta0 coefficient
    //See documentation for further explanation
    def deriveTheta0: Double = {
      var sum = 0.0
      for (i <- 0 until dataSize) {
        sum += (hypothesis(theta0 + theta1 * xData(i)) - yData(i)) 
      }
      return -(this.alpha / dataSize) * sum
    }

    //Returns the value to be added or subtracted from the theta1 coefficient
    //See documentation for further explanation
    def deriveTheta1: Double = {
      var sum = 0.0
      for (i <- 0 until dataSize) {
        sum += (hypothesis(theta0 + theta1 * xData(i)) - yData(i)) * xData(i)
      }
      return -(this.alpha / dataSize) * sum
    }
    /*
     * The main algorithm. Will do at most maxIterations size of iterations.
     * If the changes of theta0 and theta1 tend to be really small values are returned
     * and the process is stopped.    
     */
    def gradientDescent: (Double, Double) = {
      for (i <- 0 until this.maxIterations) {
        tempTheta0 = theta0 + deriveTheta0
        tempTheta1 = theta1 + deriveTheta1
        if (!(abs(theta0 - tempTheta0) > epsilon || abs(tempTheta1 - theta1) > epsilon)) {
          return (theta0, theta1)
        }
        theta0 = tempTheta0
        theta1 = tempTheta1
      }
      (theta0, theta1)
    }
    val temp = gradientDescent
    Buffer(temp._1, temp._2)
  }
}