package core

import scala.collection.mutable.Buffer

case class Tent(quota: Int, sherpas: Buffer[Sherpa], position: Coordinate, price: Int, level: Level) {
  
  val x: Float = position.x.toFloat
  val y: Float = position.y.toFloat
  
  def addSherpa(sherpa: Sherpa): Boolean = {
    if(sherpas.size < quota){
      sherpas += sherpa
      true
    } else {
      false
    }
  }
  
}