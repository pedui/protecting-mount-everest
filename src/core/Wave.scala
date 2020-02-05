package core

import scala.collection.mutable.Buffer

case class Wave(enemies: Buffer[Enemy]) {
  
  var currentEnemies: Buffer[Enemy] = enemies
  def isOver: Boolean = enemies.forall(!_.alive)
  
  def addEnemy(enemy: Enemy): Unit = {
    currentEnemies += enemy
  }
  
}