package towers.model.PathFinder

class Graph[A] {

  var nodes: Map[Int, A] = Map()
  var adjacencyList: Map[Int, List[Int]] = Map()


  def addNode(index: Int, a: A): Unit = {
    nodes += index -> a
    adjacencyList += index -> List()
  }

  def addEdge(index1: Int, index2: Int): Unit = {
    adjacencyList += index1 -> (index2 :: adjacencyList(index1))
    adjacencyList += index2 -> (index1 :: adjacencyList(index2))
  }

  def areConnected(index1: Int, index2: Int): Boolean = {
    var explored: Set[Int] = Set(index1)

    val toExplore: Queue[Int] = new Queue()
    toExplore.enqueue(index1)

    while (!toExplore.empty()) {
      val nodeToExplore = toExplore.dequeue()
      for (node <- this.adjacencyList(nodeToExplore)) {
        if(!explored.contains(node)){
          //println("exploring: " + this.nodes(node))
          toExplore.enqueue(node)
          explored = explored + node
        }
      }
    }
    explored.contains(index2)
  }

  def distance(index1: Int, index2: Int): List[Int] = {
    var explored: Set[Int] = Set(index1)
    var Distance:Map[Int,Int]=Map()
    Distance=Distance+(index1->10000000)
    val toExplore: Queue[Int] = new Queue()
    toExplore.enqueue(index1)
    while (!toExplore.empty()) {
      val nodeToExplore = toExplore.dequeue()
      for (node <- this.adjacencyList(nodeToExplore)) {
        if(!explored.contains(node)){
          toExplore.enqueue(node)
          explored = explored + node
          Distance = Distance + (node->nodeToExplore)
        }
      }
    }
    var shortestPath:List[Int]=List()
    var current=index2
    while(Distance(current)!=10000000){
      shortestPath=Distance(current)::shortestPath
      current=Distance(current)
    }
    shortestPath
  }
}
