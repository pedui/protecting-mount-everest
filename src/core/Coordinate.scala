package core

import scala.math._

case class Coordinate(x: Double, y: Double) {
  
  def length: Double = hypot(x, y)
  
  def isWithin(coord: Coordinate, edgeValue: Double): Boolean = (this - coord).length <= edgeValue
  
  def isOutside(coord: Coordinate, edgeValue: Double): Boolean = (this - coord).length > edgeValue
  
  def -(coord: Coordinate): Coordinate = Coordinate(x - coord.x, y - coord.y)
  
  def +(coord: Coordinate): Coordinate = Coordinate(x + coord.x, y + coord.y)
  
  def *(multiplier: Double): Coordinate = Coordinate(x * multiplier, y * multiplier)
  
  def normalize(): Coordinate = Coordinate(x / length, y / length)
  
  def modify(modX: Double, modY: Double) = Coordinate(x + modX, y + modY)
  
}