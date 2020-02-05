package tests

import core._
import java.io._
import org.junit.Test
import org.junit.Assert._
import org.junit.Rule
import org.junit.rules.ExpectedException._
import scala.collection.mutable.Buffer

class EverestUnitTests {
  
  val dataProcessor = new DataProcessor()
  dataProcessor.dataFolder = "testData"
      
  def addMachineToLevel(position: Coordinate): Boolean = {
    
    var loadedLevel: Level = dataProcessor.loadLevel(4)
    val loadedMachineInfo: (Double, Double, Double, Int) = dataProcessor.loadMachine
    val machine = new SnowballMachine(loadedMachineInfo._1, loadedMachineInfo._2, loadedMachineInfo._3, position,
        loadedMachineInfo._4, loadedLevel)
    
    loadedLevel.addMachine(machine)
    loadedLevel.machines.contains(machine)
  }
  
  @Test
  def testLoadValidLevelFile {
    
    val expectedNumber: Int = 4
    val expectedDefenseSpots: Buffer[Coordinate] = Buffer(Coordinate(164, 247), Coordinate(281, 550), 
        Coordinate(520, 152), Coordinate(559, 341), Coordinate(563, 621))
    val expectedPath: Buffer[Coordinate] = Buffer(Coordinate(374, 731), Coordinate(376, 720), Coordinate(381, 703),
        Coordinate(391, 679), Coordinate(403, 654), Coordinate(414, 627), Coordinate(425, 595), 
        Coordinate(438, 569), Coordinate(448, 538), Coordinate(460, 487), Coordinate(460, 441))
    val expectedMoney: Int = 100
    
    val expectedLevel = new Level(expectedNumber, expectedDefenseSpots, expectedPath, expectedMoney)
    
    val loadedLevel = dataProcessor.loadLevel(4)
    val loadedWaveEnemies = loadedLevel.waves.flatMap(_.enemies)
    
    assertEquals(expectedLevel, loadedLevel)
    assertEquals(3, loadedWaveEnemies.size)
    assertEquals(1, loadedLevel.waves.size)
  }
  
  @Test
  def testLoadValidLevelFileWithManyWaves {
    
    val expectedNumber: Int = 3
    val expectedDefenseSpots: Buffer[Coordinate] = Buffer(Coordinate(388, 597), Coordinate(596, 323), Coordinate(136, 340))
    val expectedPath: Buffer[Coordinate] = Buffer(Coordinate(374, 731), Coordinate(376, 720), Coordinate(381, 703),
        Coordinate(391, 679), Coordinate(403, 654), Coordinate(414, 627), Coordinate(425, 595), 
        Coordinate(438, 569), Coordinate(448, 538), Coordinate(460, 487), Coordinate(460, 441))
    val expectedMoney: Int = 100
    
    val expectedLevel = new Level(expectedNumber, expectedDefenseSpots, expectedPath, expectedMoney)
    
    val loadedLevel = dataProcessor.loadLevel(3)
    val loadedWaves = loadedLevel.waves
    val loadedWaveEnemies = loadedWaves.flatMap(_.enemies)
    
    assertEquals(expectedLevel, loadedLevel)
    assertEquals(15, loadedWaveEnemies.size)
    assertEquals(3, loadedWaves.size)
  }
  
  @Test
  def testLoadLevelFileWithoutEnemyWaves {
   
    val loadedLevelWaves: Buffer[Wave] = dataProcessor.loadLevel(5).waves
        
    assertEquals(1, loadedLevelWaves.size)
    assertEquals(3, loadedLevelWaves.flatMap(_.enemies).size)
  }
  
  @Test
  def testLoadLevelFileWithNotEnoughPathCoordinates {
    
    var exception: String = ""
    
    try{
      dataProcessor.loadLevel(6)
    } catch {
      case e: Exception => exception = e.getMessage
    }
    assertEquals("Level file contained an invalid amout of path coordinates (only 10 or less).", exception)
  }
  
  @Test
  def testLoadLevelFileWithoutDefenseSpots {

    var exception: String = ""
    
    try{
      dataProcessor.loadLevel(7)
    } catch {
      case e: Exception => exception = e.getMessage
    }
    assertEquals("Level file did not contain any defense spots.", exception)
    
  }
  
  @Test
  def testLoadLevelFileWithoutSetupHeader {
    
    var exception: String = ""
    
    try{
      dataProcessor.loadLevel(8)
    } catch {
      case e: Exception => exception = e.getMessage
    }
    assertEquals("Level file did not contain a setup header!", exception)
  }
  
  @Test
  def testLoadValidTentFile {
    
    val loadedTentValues: (Int, Int, Double, Double, Coordinate) = dataProcessor.loadTent
    val expectedTentValues = (4, 20, 100.0, 1.0, Coordinate(1.0, 1.0))
    
    assertEquals(expectedTentValues, loadedTentValues)
    
  }
  
  @Test
  def testLoadValidMachineFile {
    
    val loadedMachineValues: (Double, Double, Double, Int) = dataProcessor.loadMachine
    val expectedMachineValues = (10.0, 150.0, 65.0, 20)
    assertEquals(expectedMachineValues,loadedMachineValues)   
  }
  
  @Test
  def testAddMachineToLevelFreeSpot {

    assertTrue(addMachineToLevel(Coordinate(164, 247)))
  }
  
  @Test
  def testAddMachineToLevelNonDefenseSpot {
    
    assertFalse(addMachineToLevel(Coordinate(200, 200)))
  }

  @Test
  def testLoadEnemyValidFile {
    val loadedEnemy = dataProcessor.loadEnemy(1)
    val expectedHealth: Double = 100.0
    val expectedDamage: Double = 2.0
    val expectedSpeed: Coordinate = Coordinate(1.1,1.1)
    val expectedMoney: Int = 15
    
    assertTrue(expectedHealth.equals(loadedEnemy.health))
    assertTrue(expectedDamage.equals(loadedEnemy.damage))
    assertEquals(expectedSpeed, loadedEnemy.speed)
    assertEquals(expectedMoney, loadedEnemy.money)
  }
}
