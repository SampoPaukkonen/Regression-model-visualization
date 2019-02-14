package regression

import org.ejml.simple.SimpleMatrix
/*
 * A trait for all the regression models which can be represented in a matrix form.
 * The coefficients are delivered in a n x 1 coefficient vector. 
 * Lowest order coefficient (starting from the constant) is at the top of the vector.
 */
trait GeneralRegressionMatrixCalculator {
  def deliverCoefficients(): SimpleMatrix
}