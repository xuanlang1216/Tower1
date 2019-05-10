package towers.model.game_objects

import play.api.libs.json.{JsValue, Json}
import towers.model.genetics.genes.Gene
import towers.model.physics.PhysicsVector

class DodgeBallTower(val x: Int, val y: Int) extends GameObject {

  // The height at which projectiles are fired
  val height = 3.0

  // Towers can only fire at players closer than this distance from the tower
  val sightRange = 5.0

  // The magnitude of the velocity at which projectiles are fired
  val projectileVelocity = 5.0


  def fire(jsonGameState: String): List[Projectile] = {
    // TODO: Objective 2
     val towerlocation=new PhysicsVector(x+0.5,y+0.5,height)
     val parsed:JsValue=Json.parse(jsonGameState)
     val players:List[JsValue]=(parsed\"players").as[List[JsValue]]
     var cloestx:Double=10000000
     var cloesty:Double=10000000
     var cloestDistance:Double=100000000
     for(v<-players){
       var xloc=(v\"x").as[Double]
       var yloc=(v\"y").as[Double]
       var distance=Math.sqrt(Math.pow(x+0.5 - xloc, 2.0) + Math.pow(y+0.5 - yloc, 2.0))
       if(distance<cloestDistance){
         cloestDistance=distance
         cloestx=xloc
         cloesty=yloc
       }
     }
    if (cloestDistance<=sightRange){
      //cloestDistance=Math.sqrt(Math.pow(x+0.5 - cloestx, 2.0) + Math.pow(y+0.5 - cloesty, 2.0)+Math.pow(height,2))
      val direction=new PhysicsVector((cloestx-x-0.5)*projectileVelocity/cloestDistance,(cloesty-y-0.5 )*projectileVelocity/cloestDistance,0)
      List(new Projectile(new PhysicsVector(x+0.5,y+0.5,height),direction))
    }
    else {
      List()
    }
  }

  def aimFire(jsonGameState: String): List[Projectile] = {
    val towerlocation=new PhysicsVector(x+0.5,y+0.5,height)
    val parsed:JsValue=Json.parse(jsonGameState)
    val players:List[JsValue]=(parsed\"players").as[List[JsValue]]
    var cloestx:Double=10000000
    var cloesty:Double=10000000
    var cloestDistance:Double=100000000
    var cloestx_v=0.0
    var cloesty_v=0.0
    for(v<-players){
      val xloc=(v\"x").as[Double]
      val yloc=(v\"y").as[Double]
      val xvel=(v\"v_x").as[Double]
      val yvel=(v\"v_y").as[Double]
      val distance=Math.sqrt(Math.pow(x+0.5 - xloc, 2.0) + Math.pow(y+0.5 - yloc, 2.0))
      if(distance<cloestDistance){
        cloestDistance=distance
        cloestx=xloc
        cloesty=yloc
        cloestx_v=xvel
        cloesty_v=yvel
      }
    }
    if (cloestDistance<=sightRange){
      val x_dis=cloestx-towerlocation.x
      val y_dis=cloesty-towerlocation.y
      val a:Double=Math.pow(cloestx_v,2)+Math.pow(cloesty_v,2)-Math.pow(projectileVelocity,2)
      val b:Double=2*x_dis*cloestx_v+2*y_dis*cloesty_v
      val c:Double=Math.pow(x_dis,2)+Math.pow(y_dis,2)
      var t1:Double= (-b-Math.sqrt(Math.pow(b,2)-4*a*c))/(2*a)
      val t2:Double= (-b+Math.sqrt(Math.pow(b,2)-4*a*c))/(2*a)
      if(t1<0) {
        t1 = t2
      }
      val projectile_x=x_dis+cloestx_v*t1
      val projectile_y=y_dis+cloesty_v*t1
      var direction=new PhysicsVector(projectile_x/t1,projectile_y/t1,0)
      List(new Projectile(towerlocation,direction))
    }
    else {
      List()
    }
  }


  // Suggested Genetic Algorithm setup
  def getFitnessFunction(targetPlayer: Player): PhysicsVector => Double = {
    null
  }

  def vectorIncubator(genes: List[Gene]): PhysicsVector = {
    null
  }

}
