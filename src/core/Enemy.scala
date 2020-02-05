package core

class Enemy(startHealth: Double, damage: Double, val money: Int, startSpeed: Coordinate, position: Coordinate)
           extends Character(startHealth, damage, startSpeed){
  
  var currentPosition: Coordinate = position
  var speed: Coordinate = startSpeed
  
  def x: Float = currentPosition.x.toFloat
  def y: Float= currentPosition.y.toFloat
  
  var target: Sherpa = null
  var currentLevel: Level = null
  var currentPlace: Int = 0
  var actionType: String = "move"
  var hasDied: Boolean = false
  
  def levelCleared: Boolean = { 
    currentLevel.path.size == currentPlace 
  }
  
  // Scans the area for the next path coordinate and returns that new target position.
  def findTarget: Coordinate = {
    
    val targetPosition = currentLevel.path(currentPlace)

    if(currentPosition.isWithin(targetPosition, 20) && currentPlace < currentLevel.path.size){
      currentPlace += 1
    }
    targetPosition
  }
  
  //Sets the correct direction for the enemy to move in.
  def direction(target: Coordinate): Coordinate = { 

    var newDirection: Coordinate = null
    
    if(target == currentPosition){ // When the character insists on moving towards itself, we remedy that here.
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
    }
  }
  
  def attack(): Unit = {
    
    if(target.alive){ 
      
      target.health -= damage
      if(!target.alive) target.killed(System.currentTimeMillis()) // Reports the sherpas time of death
      
    } else {
      target = null
      actionType = "move"
    }  
  }
  
  /*
   *  Simplified version of the sherpa move method. The enemies goal is to move as much as possible.
   *  Helps the enemy move towards its target by adding the direction to the speed vector.
   *  The speed of the movement cannot exceed the maximum speed available (the start speed).
   */
  def move(): Unit = {      
    
    var updatedSpeed: Coordinate = (speed + direction(findTarget))
    if(startSpeed.x < updatedSpeed.length){
      updatedSpeed = updatedSpeed.normalize() * (startSpeed.x)
    }
    
    speed = updatedSpeed
    currentPosition += speed
    
    if(target != null) actionType = "attack"
  }
  
  // The dead body of the enemy is looted by the sherpas and all money found is given to the player.
  def dropMoney(): Unit = {
    if(!hasDied){
      currentLevel.currentMoney += money
      hasDied = true
    }
  }
}