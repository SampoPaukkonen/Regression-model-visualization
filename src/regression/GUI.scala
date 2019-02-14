package regression

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.canvas.Canvas
import scalafx.scene.Scene
import scalafx.scene.control.MenuBar
import scalafx.scene.control.MenuItem
import scalafx.scene.control.Menu
import scalafx.scene.control.SeparatorMenuItem
import scalafx.scene.layout.BorderPane
import scalafx.event.ActionEvent
import scalafx.stage.FileChooser
import scala.io.Source
import scalafx.scene.control.TextInputDialog
import scala.collection.mutable.Buffer
import scalafx.scene.chart.ScatterChart
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.VBox
import scalafx.stage.Stage
import scalafx.event.EventHandler
import scala.xml
import scalafx.scene.paint.Color
import scalafx.scene.text.TextFlow
import scalafx.scene.text.Text
import scalafx.scene.text.Font
import scalafx.scene.text.FontPosture
import scalafx.scene.text.FontWeight
import scalafx.scene.control.ListView
import javafx.event.EventHandler
import javafx.scene.control.Label
import scalafx.scene.layout.HBox
import scalafx.scene.control.Button
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.ButtonType



/*
 * GUI is the graphical user interface of the program.
 * It is build so that it is responsible, with help of GUIHelperMethods, for the front end side of the program.
 */
object GUI extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title = "Regression Modeling Software"
    scene = new Scene(800, 600) {
      //Style for out graph
      stylesheets.add("StylingPlot.css")
      /*
       * Below are the general file menu items.
       */
      val loader      = new Loader(Buffer[GeneralGraph]())
      val menuBar     = new MenuBar
      val modelMenu   = new Menu("Model")
      val newGraph    = new MenuItem("New Graph")
      val loadGraph   = new Menu("Load Graph")
      val modifyGraph = new Menu("Modify Graph")
      val graphInfo   = new Menu("Graph Info")
      val exit        = new MenuItem("Exit")
      val helpMenu    = new Menu("Help")
      val instructions    = new MenuItem("Instructions")
      
      /*
       * By creating the button types below it's easy to match users selection with Scala's pattern recognition.
       */
      val ButtonTypeOne   = new ButtonType("Linear Regression")
      val ButtonTypeTwo   = new ButtonType("Polynomial Regression")
      val ButtonTypeThree = new ButtonType("Logistic Regression")
      val ButtonTypeMin   = new ButtonType("New minium value")
      val ButtonTypeMax   = new ButtonType("New maximum value")

      

      modelMenu.items = List(newGraph, loadGraph, modifyGraph, graphInfo, new SeparatorMenuItem, exit)
      helpMenu.items  = List(instructions)
      menuBar.menus   = List(modelMenu, helpMenu)
      
      /*
       * These two alert types are specified in GUI rather than in GUIHelperMethods.
       * They are used for the graphs selection and for the changing of the graphs minimum and maximum values.
       */
      val modelSelectionAlert = new Alert(AlertType.Confirmation) {
        initOwner(stage)
        title = "Regression Model Selection"
        headerText = "Click to choose the model you want to draw.\nLinear regression is by definition a line. If you want a more complex plot use the polynomial regression instead.\nLogistic regression works only when the dependent variable's (y) data is binary (True of False)."
        contentText = "Choose the model"
        buttonTypes = Seq(ButtonTypeOne, ButtonTypeTwo, ButtonTypeThree, ButtonType.Cancel)
      }

      val setNewXValueAlert = new Alert(AlertType.Confirmation) {
        initOwner(stage)
        title = "Changing models x value"
        headerText = "Click to choose wheter you want to set new minimum of maximum value for the model"
        contentText = "Choose action"
        buttonTypes = Seq(ButtonTypeMin, ButtonTypeMax, ButtonType.Cancel)
      }
      //By clicking exit the session ends.
      exit.onAction = (ae: ActionEvent) => {
        sys.exit(0)
      }

      /*
       * This is the main event of GUI
       */
      newGraph.onAction = (ae: ActionEvent) => {
        /*
         * Model is first set to empty and a flag hasSelected is initialized to false
         * Flag is used to determine if the program can progress the the next stage.
         * Flag is used rather than the value of the model for clarity.
         */
        var model = ""
        var hasSelected = false
        val selection = modelSelectionAlert.showAndWait()
        selection match {
          case Some(ButtonTypeOne) =>
            model = "linear"; hasSelected = true
          case Some(ButtonTypeTwo) =>
            model = "polynomial"; hasSelected = true
          case Some(ButtonTypeThree) => 
            model = "logistic" ; hasSelected = true
          case _ =>
        }
        if (hasSelected) {
          /*
           * If the user has selected a model then a file for the data points is asked
           */
          val fileChooser  = new FileChooser
          val selectedFile = fileChooser.showOpenDialog(stage)
          var dataPointsAndNames = GUIHelperMethods.askForFileAndGetValues(selectedFile)
          /*
           * The flag canGoOn is just a safe measure if for some reason Parser has not worked properly and has given empty data or missing
           * variable name(s).
           * Can go on is set to false if such event occurs.
           */
          var canGoOn = true 
          if (dataPointsAndNames._1.isEmpty || dataPointsAndNames._2._1.isEmpty() || dataPointsAndNames._2._2.isEmpty()) {
            canGoOn = false
          }
          if (canGoOn) {
            //rawDataValues are the datapoints in the fiel
            val rawDataValues = dataPointsAndNames._1
            //yAndXNames are the variable names (first line in a correctly formatted file) of the file
            val yAndXNames = dataPointsAndNames._2
            val graphName = GUIHelperMethods.getGraphName(yAndXNames)
            //Graph value is Option[GeneralGraph]. If something goes wrong during the creation of the graph (e.g. matrix operations fail)
            //then a None value is returned.
            val graphValue = GUIHelperMethods.formGraph(model, graphName, yAndXNames, rawDataValues)
            if (graphValue.nonEmpty) {
              val graph = graphValue.get
              GUIHelperMethods.addGraph(graph, loader, loadGraph, modifyGraph, graphInfo)
              //When calculating the plot points only the interval between the minimum and maximum point is meaningful.
              //When the maximum and minimum x points of the interval is divided by the scene's current width a number representing how many
              //x values per a width pixel is obtained. This is used to calculate plot points for the graph.
              val dx       = (graph.maximumXDataPoint - graph.minimumXDataPoint) / scene.width.get
              val maximumX = graph.maximumXDataPoint
              val plot     = Plotter.drawGraph(yAndXNames._2, yAndXNames._1, loader.graphs.last, dx, maximumX)
              //Plot is created a bit lower in Y axis to be more legible.
              //Same goes for using 90% of the scene's width and height for the plot's width and height
              plot.layoutY_=(30.0)
              plot.prefHeight <== scene.height * 0.9
              plot.prefWidth <== scene.width * 0.9
              plot.autosize()
              /*
               * startItemsSize is the number of items (nodes etc.) in the GUI when it is started.
               * The only way to increment this number is by creating a new graph. 
               * When another graph is created the latest graph in the scene is removed.
               */
              if (content.size > startItemsSize) {
                content.remove(content.indexOf(content.last))
              }
              content.add(plot)
            }
          }
        }
      }

      loadGraph.onAction = (ae: ActionEvent) => {
        //for loop is used to determine which item of the loadGraph is the target of user action
        for (item <- loadGraph.items) {
          item.onAction = handle {
            //The graph which is supposed to be loaded is found via its name.
            //The same goes for the modifyGraph part.
            val toBeSelected = item.getText
            val firstToMatch = loader.graphs.find(_.name == toBeSelected)
            //Checking that there is a graph with the appropriate name
            if (!firstToMatch.isEmpty) {
              //dx is determined the same way than in the onAction part
              val dx = (firstToMatch.get.maximumXDataPoint - firstToMatch.get.minimumXDataPoint) / scene.width.get.toDouble
              val maximumX = firstToMatch.get.maximumXDataPoint//roundedMaximumXPoint(5.0)
              val yNx = firstToMatch.get.giveVariableNames
              val yName = yNx._1
              val xName = yNx._2
              val plot = Plotter.drawGraph(yName, xName, firstToMatch.get, dx, maximumX)
              plot.layoutY_=(30.0)
              plot.prefHeight <== scene.height * 0.9
              plot.prefWidth <== scene.width * 0.9
              plot.autosize()
              if (content.size > startItemsSize) content.remove(content.indexOf(content.last))
              content.add(plot)
            }
          }
        }
      }
      modifyGraph.onAction = (ae: ActionEvent) => {
        for (item <- modifyGraph.items) {
          item.onAction = handle {
            val toBeModified = item.getText
            val firstToMatch = loader.graphs.find(_.name == toBeModified)
            val graphInQuestion = firstToMatch.get
            if (!firstToMatch.isEmpty) {
              val decision = setNewXValueAlert.showAndWait()
              /*
               * If the user does a choice then appropriate measures are taken.
               * A function could be used here but with only two possibilities, and many string parameters, the structure
               * below is adequate.
               */
              decision match {
                case Some(ButtonTypeMax) => {
                  val invalidTitle   = "Invalid input"
                  val invalidContent = "A non-numeric maximum was given.\nPlease give a Double value as the maximum."
                  val content        = "Enter a new maximum value ( Double or Int) for the graphs x parameter.\nPlot will be drawn up until this value.\nData points will not be deleted"
                  val titleHeader    = "New max"
                  val oldMax = graphInQuestion.maximumXDataPoint
                  val newMax = GUIHelperMethods.userInputBox(content, titleHeader, titleHeader)
                  if (!(newMax.value == null || newMax.get.isEmpty)) {
                  try {
                    if (newMax.get.toDouble <= graphInQuestion.minimumXDataPoint) throw new InvalidFileData("New maximum can't be lower than or equal to the current minimum.")
                    if (newMax.get.toDouble.isInfinite()) throw new InvalidFileData("New maximum can't be infinity!")
                    graphInQuestion.setMaximumXDataPoint(newMax.get.toDouble)
                    val message = s"Maximum X data point changed successfully for graph ${graphInQuestion.name}.\nOld maximum was: ${oldMax}\nNew maximum is: ${newMax.get.toDouble}"
                    GUIHelperMethods.invalidDataDetected(message, "Content modification", "Change of maximum point")
                  } catch {
                    case tooSmall  : InvalidFileData                 => GUIHelperMethods.invalidDataDetected(tooSmall.description, "Content modification", "Change of maximum point")
                    case nonNumeric: java.lang.NumberFormatException => GUIHelperMethods.invalidDataDetected(invalidContent, invalidTitle, invalidTitle)
                  }
                }
                }
                case Some(ButtonTypeMin) => {
                  val invalidTitle   = "Invalid input"
                  val invalidContent = "A non-numeric maximum was given.\nPlease give a Double value as the maximum."
                  val content        = "Enter a new minimum value ( Double or Int) for the graphs x parameter.\nPlot will be drawn up until this value.\nData points will not be deleted"
                  val titleHeader    = "New min"
                  val oldMin  = graphInQuestion.minimumXDataPoint
                  val newMin  = GUIHelperMethods.userInputBox(content, titleHeader, titleHeader)
                  if (!(newMin.value == null || newMin.get.isEmpty)) {
                  try {
                    if (newMin.get.toDouble >= graphInQuestion.maximumXDataPoint) throw new InvalidFileData("New minimum can't be higher than or equal to the current maximum.")
                    if (newMin.get.toDouble.isInfinity) throw new InvalidFileData("New minimum can't be infinity!")
                    graphInQuestion.setMinimumXDataPoint(newMin.get.toDouble)
                    val message = s"Minimum X data point changed successfully for graph ${graphInQuestion.name}.\nOld minimum was: ${oldMin}\nNew minimum is: ${newMin.get.toDouble}"
                    GUIHelperMethods.invalidDataDetected(message, "Content modification", "Change of minimum point")
                  } catch {
                    case tooHigh   : InvalidFileData                 => GUIHelperMethods.invalidDataDetected(tooHigh.description, "Content modification", "Change of minimum point")
                    case nonNumeric: java.lang.NumberFormatException => GUIHelperMethods.invalidDataDetected(invalidContent, invalidTitle, invalidTitle)
                  }
                }
                }
                //If nothing is chosen then this event is consumed.
                case _ => ae.consume()
              }
            }
          }
        }
      }
      /*
       * When graphInfo is under action, the desired graph is chosen and a new stage and scene are created 
       * for its info.
       */
      graphInfo.onAction = (ae: ActionEvent) => {
        for (item <- graphInfo.items) {
          item.onAction         = handle {
            val toBeViewed      = item.getText
            val firstToMatch    = loader.graphs.find(_.name == toBeViewed)
            val graphInQuestion = firstToMatch.get
            if (!firstToMatch.isEmpty) {
              val viewingStage = new Stage {
                width  = 500
                height = 500
                title  = "Graph Info"
                val graphName  = s"Graph name: " + graphInQuestion.name
                val graphModel = graphInQuestion.toString()
                val maxX = graphInQuestion.maximumXDataPoint
                val minX = graphInQuestion.minimumXDataPoint
                val info = new Text(s"${graphName}\n\nGraphs maximum x value: ${maxX}\nGraphs minimum x value: ${minX}\n\nGraph model: ${graphModel}")
                val textFlow = new TextFlow(info)
                val hbox     = new HBox(textFlow)
                scene = new Scene {
                  content = hbox
                }
              }
              viewingStage.show()
            }
          }
        }
      }
      helpMenu.onAction = (ae: ActionEvent) => {
        GUIHelperMethods.helpWindow()
      }
      val rootPane = new BorderPane
      //Assingment of the menu bar in the scene
      rootPane.top = menuBar
      root = rootPane
      /*
       * startItemsSize is initialized for the size of items (nodes etc.) of the scene at the start.
       * By observing its value it's easy to determine of a graph has been added to the scene.
       */
      val startItemsSize = content.size
    }
  }
  
}
