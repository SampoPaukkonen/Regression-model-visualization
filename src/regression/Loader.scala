package regression

import scala.collection.mutable.Buffer
/*
 * Simple class which is responsible for storing graphs.
 * @param graphs if a buffer of type generalgraph.
 */
class Loader(val graphs: Buffer[GeneralGraph]) {
  /*
   * Method which adds the graph parameter to loaders graphs and removes any other graphs
   * which have the same name than the parameter graph.
   */
  def addGraphAndRemoveDuplicates(graph: GeneralGraph): Unit = {
    while (this.graphs.exists(_.name == graph.name)) {
      this.graphs.remove(this.graphs.indexWhere(_.name == graph.name))
    }
    this.graphs.append(graph)
  }
  
  
  
}
