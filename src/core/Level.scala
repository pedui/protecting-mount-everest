package core

import scala.collection.mutable.Buffer

case class Level(number: Int, defenseSpots: Buffer[Coordinate], path: Buffer[Coordinate], startMoney: Int) {
 
  var currentMoney: Int = startMoney
  var waveCounter: Int = 0
  var latestWaveStarted: Long = 0
  
  var machines: Buffer[SnowballMachine] = Buffer()
  var tents: Buffer[Tent] = Buffer()
  var occupiedSpots: Buffer[Coordinate] = Buffer()
  var freeSpots: Buffer[Coordinate] = Buffer() 
  var waves: Buffer[Wave] = Buffer()
 
  def isFinished(): Boolean = {
    waves.flatMap(_.enemies).exists(_.levelCleared) || waves.forall(_.isOver)
  }
  
  // All add methods check that the spot is free before placing the equipment there.
  def addTent(tent: Tent): Unit = {
    
    if(freeSpots.contains(tent.position) && currentMoney >= tent.price){
      tents += tent
      currentMoney -= tent.price
      freeSpots -= tent.position
      occupiedSpots += tent.position
    }
  }
  
  def addMachine(machine: SnowballMachine): Unit = {
    
    if(freeSpots.contains(machine.position) && currentMoney >= machine.price){
      machines += machine
      currentMoney -= machine.price
      freeSpots -= machine.position
      occupiedSpots += machine.position
    }
  }
  
  def addWave(wave: Wave): Unit = {
    
    waves += wave
  }
}