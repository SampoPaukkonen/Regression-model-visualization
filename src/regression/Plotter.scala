package regression

import scalafx.scene.chart.ScatterChart
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.XYChart
import scalafx.scene.chart.NumberAxis

/*
 * Object used to plot i.e. to create a scatter chart for the graph
 * @param xName name of the x axis. Variable name for x is used.
 * @param yName name of the y axis. Variable name for y is used.
 * @param graph the grap to be plotted.
 * @param step i.e. dx of for calculating the plot points.
 * @param maximumX the upper bound to which plot points are calculated
 * 
 * The plot is not forced to start from zero. This is because in some graphs, for example in
 * logistic, the actual plot can be way off from the origin (min x is around 700 per se).
 * User can force zero in range by setting the minimum value of the graph as zero.
 * 
 * The plotted graph is only continuous from its minimum to its maximum.
 * Due to properties of either NumberAxis or ScatterChart, the visible range of x values is always a bit longer than what
 * user specifies. Because of this the graph can appear as disjoint at the beginning and the the end of the x axis. 
 */

object Plotter {

  def drawGraph(xName: String, yName: String, graph: GeneralGraph, step: Double, maximumX: Double): ScatterChart[Number, Number] = {
    val xAxis = NumberAxis()
    xAxis.upperBound_=(graph.maximumXDataPoint)
    xAxis.lowerBound_=(graph.minimumXDataPoint)
    xAxis.forceZeroInRange_=(false)
    val yAxis = NumberAxis()
    yAxis.upperBound_=(graph.estimateY(graph.maximumXDataPoint))
    yAxis.lowerBound_=(graph.estimateY(graph.minimumXDataPoint))
    val originalData = XYChart.Series[Number, Number](
      yName,
      ObservableBuffer(graph.dataPointsUntilMaxX(graph.maximumXDataPoint).map(z =>  XYChart.Data[Number, Number](z._2, z._1)): _*)) //Values have to be in order x, y
    val model = XYChart.Series[Number, Number](
      xName,
      ObservableBuffer(graph.calculatePlotPoints(step, graph.minimumXDataPoint, graph.maximumXDataPoint).map(z => XYChart.Data[Number, Number](z._2, z._1)): _*))
    val temp = new ScatterChart(xAxis, yAxis, ObservableBuffer(model, originalData))
    temp.XAxis.label_=(xName)
    temp.YAxis.label_=(yName)
    temp
  }

}
