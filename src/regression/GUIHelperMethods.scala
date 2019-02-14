package regression

import scalafx.Includes._
import scalafx.scene.control.TextInputDialog
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.text.Text
import scalafx.stage.Stage
import scalafx.scene.text.TextFlow
import scalafx.scene.layout.HBox
import scalafx.scene.Scene
import scalafx.scene.control.ListView
import scalafx.beans.property.ObjectProperty
import scala.collection.mutable.Buffer
import java.io.File
import scalafx.scene.control.Menu
import scalafx.scene.control.MenuItem
import scalafx.scene.control.Button
import scalafx.event.ActionEvent
import scalafx.scene.control.ButtonType
import scalafx.scene.input.MouseEvent

//Simple error class which is used throughout the program.
case class InvalidInput(description: String)    extends java.lang.Exception(description)
case class InvalidFileData(description: String) extends java.lang.Exception(description)


object GUIHelperMethods {
  
  /*
   * formGraph is a simple method which matches the model specified by the user in the GUI to an available graph model here.
   * There is no wildcard in the match structure because it is impossible to give anything other than linear, polynomial or logistic as the
   * value of the model via the graphical user interface.
   * @param model is the regression model to be used in GUI. GUI gives the model based on what user chooses.
   * @param graphName is the name of the graph.
   * @param yAndXNames are the names of the dependent (y) and the independent (x) variable.
   * @param rawDataValues if a buffer consisting of the data values which the created graph is supposed to model.
   * Return value is Option[GeneralGrap] since with the use of matrix operations in polynomial and linear graph the is a danger
   * that a matrix can't be inverted. This is most likely due to number overflow is the rawDataValues contains too large values.
   */
  def formGraph(model: String, graphName: String, yAndXNames: (String, String), rawDataValues: Buffer[(Double, Double)]): Option[GeneralGraph] = {
    try {
      model match {
      case "linear" => {
        val polynomialOrder = 2
        Some(new LinearGraph(graphName, yAndXNames, polynomialOrder, rawDataValues))
      }
      case "polynomial" => {
        var polynomialOrder = 2
        polynomialOrder = GUIHelperMethods.getGraphOrder()
        Some(new PolynomialGraph(graphName, yAndXNames, polynomialOrder, rawDataValues))
      }
      case "logistic" => {
        Some(new LogisticGraph(graphName, yAndXNames, rawDataValues))
      }
    }
   } catch {
     case (e: java.lang.IllegalArgumentException) => {
       GUIHelperMethods.invalidDataDetected("Graph coefficient calculations have failed due to overflow caused by too large values in the given data\nand/or too large polynomial order (if one is used).\nPlease check the values in the data and rescale them if possible.", "File content exception", "Coefficient calculations have failed")
       None
     }
   }
  }
  
  
  /*
   * @param graph is the graph which is to be added to the loader object and all the menus of the GUI
   * which need it
   * @param loader is the object which stores all the graphs created in a single session.
   * @param menu is unspecified number of menus to which the graph is to be added.
   */
  def addGraph(graph: GeneralGraph, loader: Loader, menu: Menu*) = {
    //loader takes care that all the graphs in it have unique names.
    loader.addGraphAndRemoveDuplicates(graph)
    val menus = menu.toVector
    for (Menu <- menus) {
      if (!Menu.items.exists(z => z.getText == graph.name)) {
        Menu.getItems.add(new MenuItem(graph.name))
      }
    }
  }
  
  /*
   * @param file is the file from which the values are to be extracted.
   * The method recognizes if no file is given to it or if too little data (no data points) is found
   * in the file given.
   */
  def askForFileAndGetValues(file: File): (Buffer[(Double, Double)], (String, String)) = {
    val inputFile = file
    var dataPointsAndNames = (Buffer[(Double, Double)](), ("", ""))
    val title      = "File content alert"
    //inputFile is null value of no file is chosen by the user
    if (inputFile != null) {
      try {
        dataPointsAndNames = DataCollector.chooseTypeAndExtractFromFile(inputFile)
        if (dataPointsAndNames._1.isEmpty) {
          throw new InvalidFileData(s"No data points were found.")
        }
      } catch {
        case invalid: InvalidFileData => invalidDataDetected(invalid.description, title, title)
      }
    }
    dataPointsAndNames
  }
  /*
   * A simple method which is used throughout the program. Allows user the give textual input.
   * @param content is the content string of the input box
   * @param title is the title of the input box
   * @param header is the header of the input box
   */
  def userInputBox(content: String, title: String, header: String): ObjectProperty[String] = {
    val inputBox             = new TextInputDialog
    inputBox.contentText     = content
    inputBox.titleProperty() = title
    inputBox.headerText      = header
    inputBox.showAndWait()
    inputBox.result
  }
  /*
   * Simple method which ask the name of the graph.
   * @param variableNames is the names of the dependent and independent variable.
   * They are used as default names if user gives no name for the graph. 
   */
  def getGraphName(variableNames: (String, String)): String = {
    val content           = "Please give name to your graph.\nUnnamed graphs will use variable names as their name.\nAny duplicates will be removed automatically so you want to give special name to your graphs."
    val titleHeader       = "Graph Name"
    val userSpecifiedName = userInputBox(content, titleHeader, titleHeader)
    var graphName         = ""
    /*
     * If user presses cancel button a null value is returned.
     */
    if (userSpecifiedName.value == null || userSpecifiedName.get.isEmpty) {
      graphName = s"${variableNames._1} in terms of ${variableNames._2}"
    } else {
      graphName = userSpecifiedName.get
    }
    graphName
  }
  /*
   * Method used for the polynomial graphs order. The method for the input value until one is given to it.
   */
  def getGraphOrder(): Int = {
    val titleHeader        = "Graph Order"
    val inputOrderText     = "Please give the order of the polynomial for the graph\nNote that your input is n - 1 meaning that input of 2 would result in a first order polynomial\nAccepted values are 2 to 90 inclusive\nGive only integers."
    var userSpecifiedOrder = GUIHelperMethods.userInputBox(inputOrderText, titleHeader, titleHeader)
    /*
     * Below a flag is created to keep the while loop going until desired input value is given.
     */
    var orderInputFlag = false
    while (!orderInputFlag) {
      try {
        if (userSpecifiedOrder.value == null || userSpecifiedOrder.get.toInt < 2 || userSpecifiedOrder.get.toInt >= 90) {
          throw new InvalidInput(s"Too small or large order was given")
        }
        orderInputFlag = true
      } catch {
        case nonNumeric: java.lang.NumberFormatException => userSpecifiedOrder = GUIHelperMethods.userInputBox(inputOrderText, titleHeader, titleHeader)
        case tooSmall: InvalidInput                      => userSpecifiedOrder = GUIHelperMethods.userInputBox(inputOrderText, titleHeader, titleHeader)
      }
    }
    userSpecifiedOrder.get.toInt
  }
  
  /*
   * Purpose of this function is to notify the user that the file they gave contains wrong or invalid information.
   * It's also a general purpose function which can be used to notify the user about anything.
   */
  
  def invalidDataDetected(content: String, titleString: String, header: String) = {
    new Alert(AlertType.Information) {
      title       = titleString
      headerText  = header
      contentText = content
    }.showAndWait()
  }
  /*
   * Method which creates a new separate stage and scene for the help window in GUI
   * Topics and their corresponding legends are in a map.
   * This map is used in the helpStage's scene, in which a functional style listView is created.
   * When user presses a topic it's legend is shown.
   */
  def helpWindow() = {
      val helpStage = new Stage() {
          width = 600
          title = "A wild help stage appears!"
          val defaultText = "For instructions\nchoose a topic"
          val helpText    = new Text(defaultText)
          val optionText  = Map(
              "Creating plot"    -> "In order to plot data you must first create a graph.\nThat can be done by pressing \"File\" -> \"New Graph\".\nAfter pressing \"New Graph\" a window opens up for\nselecting the file you want to plot.\nSee section \"Loading file\" for more information.\n\nAfter selecting the desired file you are asked to give\nthe highest order for the polynomial in the model.\nDoubles are rounded down and the minimum order is 2.\nSee section \"Calculating plot\" for more information.\n\nAfter you have given the order you are then asked to give\na name for your graph. If no name is given then a generic\nname is constructed from the variable names in the form of\n\"\"\"Y in terms of X\"\"\", where Y is the name of the y variable\nand X the name of the x variable.\n\nCongratulations! Now your plot has been drawn.\nSee section \"Managing Graphs\" for more information.",
              "Loading file"     -> "The program supports CSV and XML formats.\nIn CSV file the first pair of values in considered\nto be the names of y and x variables respectively.\n\nOnly first two values in any line are considered to be\nthe data values.\nIf non-numeric data - i.e. anything that can't be converted\nto Double - is found drawing process is terminated\nand user is prompted with a warning message.\nWarning message also arrises when an uneven number of\ndata points is found (means that one or the other is missing).\n\nXML files need to follow a certain structure.\nAs for most XML files, the XML version should be specified at\nthe top of the file. Tests have shown no problem with files\nmissing the XML version declaration but declaration is still\nrecommended. The file must a proper XML structure meaning\nthat any missing closing tags etc. will cause the program to\nwarn user. For the XML structure see documentation.",
              "Calculating linear and polynomial regression" -> "For linear and polynomial regression matrix algebra is used to\nderive coefficients.\nWhen k is the highest polynomial order and n is the number\nof data points, the n equations can be written as\n y_1 = b + a_1 * x_1 + a_2 * (x_1)^(2) + ... + a_k * (x_1)^(k)\n y_2 = b + a_1 * x_2 + a_2 * (x_2)^(2) + ... + a_k * (x_2)^(k)\n.\n.\n.\n y_n = b + a_1 * x_n + a_2 * (x_n)^(2) + ... + a_k * (x_n)^(k).\nThis can be represented as Y = B * X\nwhere Y is the dependent variable vector,\nB is coefficient vector and X is the independent variable\nmatrix.\nTask is to find B, so with matrix algebra, namely using the\ninverse of X, we can represent B as\nB = (X^(T)*X)^(-1)*X^(T) * Y.",
              "Calculating logistic regression" -> "Finding coefficients for logistic regression is more complex.\nLogistic regression can be expressed as\nh_theta(x) = 1 / (1 + e^(-theta * x)), where theta is the\ncoefficient vector for the independent variables and x is the \nattribute vector for each independent variable.\nFinding the coefficients is done by the gradient descent\nalgorithm which uses partial derivatives to find the\ncoefficients.",
              "Managing Graphs"  -> "When you have created a Graph you can load it by\nclicking \"File\" and howering your mouse over \"Load Grap\".\n\nList view should open next to the \"File\" menu containing all\nthe graphs you have plotted in this session.\n\nWhen you exit you lose all the plotted graphs.\nWhen asked for a name you should preferably always give\na unique name for the graph or make sure that the variable\nnames are themselves unique.\nThis is due to the fact that NO name duplicates are allowed\ni.e. if the name of your first graph is A and you make a new\ngraph (new order, new points etc) with name A, then the\nprevious graph A is now replaced.",
              "Modifying Graphs" -> "After you have created a plot you can modify its maximum\nx value. This can be done by \"Model\" -> \"Modifying Graph\"\nand clicking the graphs name you want to modify.\nOnce pressed a small window will appear where you have\nthe option to set a new min or max x value for the graph.\nIn order to see the change you have to reload the graph.",
          ).withDefaultValue(defaultText)
          scene = new Scene {
            val listView = new ListView(optionText.keySet.toSeq) {
              selectionModel().selectedItem.onChange {(_, _, selected) => 
                helpText.text = optionText(selected)
              }
            }
            val textFlow = new TextFlow(helpText)
            val hbox     = new HBox(10, listView, textFlow)
            content      = hbox
          }
        }
        helpStage.show()
  }
  
}
