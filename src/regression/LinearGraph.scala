package regression

import scala.collection.mutable.Buffer

/*
 * Class representing the graph of polynomial regression.
 * @param name is the name of the graph
 * @param variableNames are the names of the dependent and independent variable
 * @param polynomial order is the highest polynomial order to be used in this graph.
 * Note that polynomial orders are interpret as n - 1, so order of 2 would be 1 (line).
 * LinearGraph has by default polynomialOrder of 2.
 * @param originalData are the data points which this graph is supposed to model.
 * 
 */
class LinearGraph(name: String, variableNames: (String, String),
    polynomialOrder: Int = 2,
    private var originalData: Buffer[(Double, Double)])
    extends PolynomialGraph(name,
        variableNames,
        polynomialOrder,
        originalData) {
}