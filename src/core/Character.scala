package core

abstract class Character (startHealth: Double, val damage: Double, speed: Coordinate) {
  
  def x: Float
  def y: Float
  
  var health: Double = startHealth 
  
  val maxHealth: Double = startHealth
  
  def alive: Boolean = {
    health > 0
  }

  def attack
  
  def move
}