package regression

import scala.collection.mutable.Buffer
import org.ejml.simple.SimpleMatrix
import scala.math.pow

class RegressionMatrixCalculator(val inputData: Buffer[(Double, Double)], val polynomialOrder: Int) extends GeneralRegressionMatrixCalculator {

  /*
   * InputData is given in the form of Buffer[(Double, Double)] where the first element in a pair
   * (a, b) is the y value and the second element is the x value.
   */
  def deliverCoefficients(): SimpleMatrix = {
    val unZippedData = inputData.clone().unzip
    
    /*
     * y values are made into a vector and x values are put into a matrix.
     */
    val xValues = Array(List.fill(inputData.size)(1.0).toArray, unZippedData._2.toArray)
    val yValues = Array(unZippedData._1.toArray)
    val Y       = new SimpleMatrix(yValues).transpose //Y is essentially a  n x 1 vector.
    
    /*
     * Matrix X looks like [1, x_i, x_i^2, x_i^3,...,x_i^m]
     * 										 [..............................]
     * 										 .                              .
     * 										 .                              .
     * 										 .                              .
     * 										 [1, x_n, x_n^2, x_n^3,...,x_n^m]
     * 
     * where m is the polynomialOrder (has to be atleast 2 to be a line)
     * and   n is the number of data points.
     */
    val X = new SimpleMatrix(xValues(0).size, polynomialOrder)
    for (i <- 0 until X.numRows()) {
      X.set(i, 0, xValues(0)(i))            //xValues(0) = Array(1, 1, 1, 1,.....)
      for (k <- 1 until X.numCols()) {
        X.set(i, k, pow(xValues(1)(i), k))  //xValues(1) = Array(x_1, x_2, x_3,...)
      }
    }
    
    /*
     * Inverse of X is the form of (X^(T)*X)^(-1)*X^(T) which is a Moore-Penrose pseudoinverse matrix.
     */
    val invertedX = X.pseudoInverse()
    
    /*
     * Here B is the coefficient vector in the form of [b  ]
     * 																								 [a_1]
     * 																								 [a_2]
     * 																								 .   .
     * 																								 .	 .
     * 																								 .	 .
     * 																								 [a_m]
     * where b is constant and a_1,...,a_m are coefficients for the corresponding order of the polynomial.
     */
    val B = invertedX.mult(Y)
    B
  }

}
