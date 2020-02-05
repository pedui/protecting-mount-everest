package core

class Sherpa(startHealth: Double, tent: Tent, damage: Double, startSpeed: Coordinate, startPosition: Coordinate)
            extends Character(startHealth, damage, startSpeed) {
  
  var currentPosition: Coordinate = startPosition
  var speed: Coordinate = startSpeed
  
  def x: Float = currentPosition.x.toFloat
  def y: Float = currentPosition.y.toFloat   
  
  var timeOfDeath: Long = 0 
  val range: Double = 80.0
  var actionType: String = "patrol"
  var target: Enemy = null
  val nearTentPosition: Coordinate = tent.level.path.minBy(coord => (startPosition - coord).length)
  
  def levelEnemies() = tent.level.waves.flatMap(_.enemies)
  
  /*
   *  Scans the area for possible enemies, sets them as the target if they are within range and
   *  returns that target.
   */
  def findTarget(): Enemy = {
    val nearestEnemies = levelEnemies.filter(enemy => 
                         enemy.alive && currentPosition.isWithin(enemy.currentPosition, range) )
    
    val targetSelection = nearestEnemies.sortBy(enemy => (currentPosition - enemy.currentPosition).length).headOption
                    
    if(!targetSelection.isDefined) return null
    
    target = targetSelection.get
    actionType = "patrol"
    
    target
  }
  
  def targetIsClose(): Boolean = {
    
    if( currentPosition.isWithin(target.currentPosition, range / 2 ) ){
      
      target.target = this
      actionType = "attack"
      true
    } else {
      false
    }
  }
  
  //Sets the correct direction for our sherpa to move in.
  def direction(target: Coordinate): Coordinate = { 
    
    var newDirection: Coordinate = null
    
    if(target == currentPosition){
      newDirection = (target - currentPosition.modify(0.6, 0.6))
    } else {
      newDirection = (target - currentPosition)
    }
    newDirection.normalize * speed.length
  }
  
  def action(): Unit = {
    actionType match {
      case "move" => {
        move
      }
      
      case "attack" => {
        attack
      }
      
      case "patrol" => {
        patrol
      }
      
      case "guardTent" => {
        guardTent
      }
    }  
  }
  
  /*
   *  Helps our sherpa move towards its target by adding the direction to the speed vector.
   *  The speed of the movement cannot exceed the maximum speed available (the start speed).
   */
  def move(): Unit = {
    
    if(target != null && currentPosition.isWithin(target.currentPosition, 80)){
      
      var updatedSpeed = speed + direction(target.currentPosition)
      if(startSpeed.x < updatedSpeed.length) updatedSpeed = updatedSpeed.normalize() * (startSpeed.x)
      
      speed = updatedSpeed
      currentPosition += speed
    }
    targetIsClose
  }
  
  def attack(): Unit = {
    
    if(targetIsClose && target.alive){
      target.health -= damage
    } else {
      target = null
      actionType = "patrol"
    }
  }
  
  /*
   *  The sherpa recovers slowly while patrolling for potential enemies. It will patrol and guardTent
   *  while there are no enemies around. This will make sure it stays on the lookout but doesn't stray
   *  too far away from its tent and leave the area unprotected.
   */
  def patrol(): Unit = {
    
    if(maxHealth > health) health += 0.15.min(maxHealth - health)
    
    if(findTarget != null) {
      actionType = "move"      
    } else {
      actionType = "guardTent"
    }
  }
  
  def guardTent(): Unit = {
    
    if(currentPosition.isOutside(nearTentPosition, 10)){
      
      var updatedSpeed = speed + direction(nearTentPosition)
      if(startSpeed.x < updatedSpeed.length){
        updatedSpeed = updatedSpeed.normalize() * (startSpeed.x)
      }
      
      speed = updatedSpeed
      currentPosition += speed
      
    } else {
      actionType = "patrol"
    }
  }

  // The enemy calls this method after killing the sherpa and sets the new time of death.
  def killed(timeKilled: Long): Unit = {
    timeOfDeath = timeKilled
  }
  
  // The sherpa will revive itself after a power nap (it takes 5 seconds to be exact) in its tent.
  def revive(): Unit = {
   
    val currentTime: Long = System.currentTimeMillis()
    if( (currentTime - timeOfDeath) >= 5000){
      health = startHealth
      timeOfDeath = 0
      currentPosition = startPosition
    }
  }
  
}