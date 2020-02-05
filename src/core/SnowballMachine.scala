package core

case class SnowballMachine(damage: Double, range: Double, hitsPerMinute: Double, position: Coordinate, price: Int, level: Level) {
  
  val x: Float = position.x.toFloat
  val y: Float = position.y.toFloat
  
  var target: Enemy = null
  var hasRangeVisible: Boolean = false
  var timeLastShot: Double = System.currentTimeMillis()
  var timeBetweenShots: Long = 60000 / hitsPerMinute.toLong
  
  def levelEnemies() = level.waves.flatMap(_.enemies)
  
  // This method makes it possible to draw the range of this specific machine on the level map.
  def toggleRangeVisibility(): Unit = {
    hasRangeVisible match {
      case false => {
        hasRangeVisible = true
      }
      case true => {
        hasRangeVisible = false
      }
    }
  }
  
  /*
   *  Scans the area for possible enemies, sets them as the target if they are within range and
   *  returns that target. It has the same structure in class Sherpa.
   */
  def findTarget(): Enemy = {
    
    val nearestEnemies = levelEnemies.filter(enemy => 
                         enemy.alive && position.isWithin(enemy.currentPosition, range.toFloat) )
    
    val targetSelection = nearestEnemies.sortBy(enemy => (position - enemy.currentPosition).length).headOption
                    
    if(!targetSelection.isDefined) return null
    
    target = targetSelection.get
    target
  }
  
  /*
   *  As the SnoWizard2000 is a simple and sweet device, it constantly scans for targets and attacks the
   *  newest target found. The target gets hit according to the timeBetweenShots calculated from the
   *  hitsPerMinute given in the data folder of the enemy type.
   */
  def action(): Boolean = {
    
    findTarget
    
    if(target != null){
      
      val currentTime: Long = System.currentTimeMillis()
      
      if(target.alive && position.isWithin(target.currentPosition, range.toFloat)
          && (currentTime - timeLastShot) >= timeBetweenShots){
      
        timeLastShot = currentTime
        return true
      }
      if(!target.alive) target = null
    }
    
    false
  }
  
}