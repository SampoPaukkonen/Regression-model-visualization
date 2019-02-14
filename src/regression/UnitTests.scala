package regression

import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.Buffer
import scala.math.pow
import scala.math.abs


class UnitTests {
  
  def estimate(cof: Buffer[Double], value: Double): Double = {
      var sum: Double = 0.0
      for (i <- cof.indices) {
        sum += cof(i) * pow(value, i)
      }
      sum
    }
  val testData1 = Buffer((-1, 1), (29, 3), (1, -11), (7, -2), (-8, -18), (-28, 22), (-15, -30), (19, 8), (-17, 27), (-22, 5))
  val testData2 = Buffer((46, -1), (-17, -11), (9, 16), (-4, -6), (32, -24), (39, 17), (35, -5), (38, -27), (-32, -18), (36, 17))
  val testData3 = Buffer((-34, -48), (-68, 14), (-70, 65), (13, 32), (26, 10), (-53, 56), (-18, 46), (-55, 45), (5, 18), (58, -70))
  val testData4 = Buffer((8, -49), (-61, -36), (-26, 56), (-21, 36), (64, 58), (-27, -20), (53, 46), (61, 34), (23, 45), (-53, -26))
  val testData5 = Buffer((-45, 46), (-35, 54), (-55, 51), (-60, 16), (-85, -5), (-62, -84), (-10, 27), (44, -20), (72, 31), (64, -24))
  val testDatas = Buffer(testData1, testData2, testData3, testData4, testData5).map(z => z.map(x => (x._1.toDouble, x._2.toDouble)))
  val linearCoefficients = Buffer((-3.4322, -0.1356), (19.82483, 0.38686), (-9.87882, -0.57864), (-7.44377, 0.66276), (-16.67199, -0.05739))
  
  @Test def linearCoefficient() {
    for (i <- testDatas.indices) {
      val correctValues = linearCoefficients(i)
      val operator      = new RegressionMatrixCalculator(testDatas(i), 2)
      val query         = operator.deliverCoefficients()
      assertEquals("linear coefficients constant match calculated constant: ", correctValues._1, query.get(0, 0), 0.001)
      assertEquals("linear coefficients slope mach calculated slope: ", correctValues._2, query.get(1, 0), 0.001)
    }
  }
  /*
   * Quick test to see that the highest order coefficient in the polynomial graph is one, when the n equations are y = 1 * x^(i) for i = 1, 2,...,n
   */

  val howMany = 50
  @Test def TestHighestPolCoefficient() {
    for (i <- 0 until howMany) {
      val testData = Buffer.tabulate(500)(n => (pow(n, i + 1).toDouble, n.toDouble))
      val operator = new RegressionMatrixCalculator(testData, i + 2)
      val query    = operator.deliverCoefficients()
      assertEquals("polynomial coefficients highest order match: ", 1, query.get(query.numRows() - 1, 0), 0.0000000001)
    }
  }
  
  @Test def allCoefficientsMatch() {
    val buffSize = 4
    import scala.util.Random
    val rand     = new Random
    val coefficientBuffer: Buffer[Buffer[Double]] = Buffer()
    for (i <- 0 until buffSize) {
      var a = Buffer[Double]()
      for (j <- 0 until buffSize) {
        a.append(rand.nextInt(buffSize).toDouble)
      }
      coefficientBuffer.append(a)
    }
    for (i <- coefficientBuffer.indices) {
      def estimate(buff: Buffer[Double], value: Double): Double = {
        var sum = buff(0)
        for (i <- 1 until buff.size) {
          sum += buff(i) * pow(value, i)
        }
        sum
      }
      val data      = Buffer.tabulate(3000)(n => (estimate(coefficientBuffer(i), n / 1000.0), (n / 1000.0).toDouble))
      val query     = new RegressionMatrixCalculator(data, coefficientBuffer.size).deliverCoefficients()
      for (j <- 0 until query.numRows()) {
        assertEquals(s"polynomial coefficient of order ${j} match: ", coefficientBuffer(i)(j), query.get(j, 0), 0.1)
      }
    }
  }
  
  
  
  
  
  
  
  
  
  
  
}

