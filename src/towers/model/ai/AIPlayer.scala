package towers.model.ai

import play.api.libs.json.{JsValue,Json}
import towers.model.GridLocation
import towers.model.physics.PhysicsVector
import towers.model.PathFinder.Graph

class AIPlayer(id: String) {

  def computeMovement(jsonGameState: String): PhysicsVector = {
    // Do not edit this method. It will not be called during grading
    var path = getPath(jsonGameState)
    path = smoothPath(jsonGameState, path)
    pathToDirection(jsonGameState, path)
  }

  def getPath(jsonGameState: String): List[GridLocation] = {
    //TODO
    val pathgraph:Graph[GridLocation]=new Graph()
    val parsed:JsValue=Json.parse(jsonGameState)
    val players=(parsed\"players").as[List[JsValue]]
    var AIx:Int=0
    var AIy:Int=0
    for(p<-players){
      if((p\"id").as[String]==id){
        AIx=(p\"x").as[Double].toInt
        AIy=(p\"y").as[Double].toInt
      }
    }
    val walls=(parsed\"walls").as[List[JsValue]]
    var the_wall:List[GridLocation]=List()
    var nodeindex=0
    for(w<-walls){
      the_wall=new GridLocation((w\"x").as[Int],(w\"y").as[Int])::the_wall
    }
    val base=(parsed\"base").as[Map[String,Int]]
    val startloc=(parsed\"start").as[Map[String,Int]]
    val gridsize=(parsed\"gridSize").as[Map[String,Int]]
    val gridHeight=gridsize("y")
    val gridWidth=gridsize("x")

    for(y<- 0 until gridHeight){
      for(x<-0 until gridWidth){
        pathgraph.addNode(nodeindex,new GridLocation(x,y))
        nodeindex+=1
      }
    }
    for((node,grid)<-pathgraph.nodes){
      pathgraph.addEdge(node,node+1)
      pathgraph.addEdge(node,node-1)
      pathgraph.addEdge(node,node-gridWidth)
      pathgraph.addEdge(node,node+gridWidth)
    }






    List()
  }

  def NotAWall(x:Int,y:Int,wall:List[GridLocation]):Boolean={
    for(w<-wall){
      if(x==w.x && y==w.y){
        false
      }
    }
    true
  }

  def pathToDirection(jsonGameState: String, path: List[GridLocation]): PhysicsVector = {
    // TODO
    new PhysicsVector(Math.random() - 0.5, Math.random() - 0.5)
  }


  def smoothPath(jsonGameState: String, path: List[GridLocation]): List[GridLocation] = {
    // TODO
    path
  }

}
