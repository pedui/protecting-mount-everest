package core

import java.io._
import scala.collection.mutable.Buffer
import scala.util.Random

  /* 
   * Here we have all the data reader methods of the game. The reading is very straightforward, as
   * it constantly repeats the same pattern in every load method.
   * Every single line is saved into a Buffer and processed one by one. All lines starting with "-" 
   * are interpreted to contain set headers. A set header has specific information headers that
   * follow it. The load method processes the information following the information headers.
   * The end result is either a selection of values that match the class associated with the load
   * file or a true instance of that class.
   */

class DataProcessor {
  
  var dataFolder: String = "data"
  
  /* 
   * The loadLevel method is the most important method of the game. Without it there won't be a functioning
   * level. It reads a level file from the data folder in the project root and returns a Level instance based
   * on the information written in that file. It is also the method most vulnerable to any errors, as
   * it does not have default values for paths, defenseSpots and waves.
  	 */
  def loadLevel(number: Int): Level = {
    
    val filename = s"$dataFolder/Level $number"
    var level: Level = null
    
    try {
      val file: FileReader = new FileReader(filename)
      val reader: BufferedReader = new BufferedReader(file)
      
      var currentLine: String = reader.readLine()
      var lineStorage = Buffer[String](currentLine + "\n")
      
      while(currentLine != null){
        currentLine = reader.readLine()
        lineStorage += currentLine + "\n"
      }
      
      var cleanedStorage: String = ""
      if(!lineStorage.head.contains("-")){
        cleanedStorage = lineStorage.dropRight(1).drop(1).mkString
      } else {
        cleanedStorage = lineStorage.mkString
      }
      
      val infoSets: Array[Array[String]] = cleanedStorage.split("-").drop(1).map(_.split("\n"))
      
      var setupHeaderFound = false
      
      var levelMoney = 100
      var levelPath = Buffer[Coordinate]()
      var defenseSpots = Buffer[Coordinate]()
      var levelWaves = Buffer[Wave]()
      
      for(set <- infoSets) {
        
        val setHeader = set(0).toLowerCase.trim.replaceAll("\\s","")

        setHeader match {
          
          case "setup" => {
            
            setupHeaderFound = true
            
            for(information <- set){
              
               val infoArray: Array[String] = information.split(":").map(_.toLowerCase.trim)
               val infoHeader: String = infoArray(0) 
               
               if(infoArray.size > 1 && !infoArray(1).isEmpty()){
               
                 try {
                  
                   infoHeader match {
                     
                     case "money" => {

                       levelMoney = infoArray(1).trim.toInt 
                     }
                     
                     case "defensespots" => {
                       
                       var spots = Buffer[Coordinate]()
                       
                       for(coordinates <- infoArray.drop(1).mkString.split("#")) {
                         
                         val coord = coordinates.trim.split(",").map(_.trim.toDouble)
                         
                         if(coord(0) < 740 && coord(1) < 740){
                           spots += new Coordinate(coord(0), coord(1)) 
                         }
                       }
                       defenseSpots = spots
                     }
                     
                     case "path" => {
  
                       for(coordinates <- infoArray.drop(1).mkString.split("#")){
                         
                         val coord = coordinates.split(",").map(_.trim.toDouble)
                         
                         if(coord(0) < 740 && coord(1) < 740) {
                           levelPath += new Coordinate(coord(0), coord(1))
                         }
                       }
                     }
                     
                     case _ => {
                       
                     }
                   }
                     
                 } catch {
                   case e: Throwable => 
                 }
                 
               }
             }
          }
            
          case "wavesettings" => {
            
            for(information <- set){
              
              val infoArray: Array[String] = information.split(":").map(_.toLowerCase.trim)
              val infoHeader: String = infoArray(0)
              
              if(infoArray.size > 1 && !infoArray(1).isEmpty()){
              
                 try {
                
                   infoHeader match {
                       
                     case "wave" => {
                      
                       val waveInfo = infoArray(1).split(",").map(_.toLowerCase.trim)
                       
                       if(waveInfo.size == 2 && waveInfo(1).toInt < 21){
                         val levelEnemies: Buffer[Enemy] = Buffer.fill(waveInfo(1).toInt)(loadEnemy(waveInfo(0).toInt))
                         levelWaves += new Wave(levelEnemies)
                       }
                     }
                     
                     case _ => {
  
                     }
                   
                   }
                   
                 } catch {
                   case e: Throwable => 
                 }
               }
             }
          }
          
          case _ => {

          }
        }
      }
      
      if(!setupHeaderFound) throw new Exception("Level file did not contain a setup header!")
      
      if(levelPath.size <= 10){
        throw new Exception("Level file contained an invalid amout of path coordinates (only 10 or less).")
      }
      if(defenseSpots.size == 0){
        throw new Exception("Level file did not contain any defense spots.")
      }
      if(levelWaves.size == 0 || levelWaves.size > 10) {
        val enemies: Buffer[Enemy] = Buffer.fill(3)(loadEnemy(1))
        levelWaves += new Wave(enemies)
      }
      
      level = new Level(number, defenseSpots, levelPath, levelMoney)
      level.freeSpots = defenseSpots

      levelWaves.foreach{
        wave => 
        for(enemy <- wave.enemies) {
          enemy.currentLevel = level
          enemy.currentPosition = level.path(0).modify(0, Random.nextInt(50))
        }
        level.addWave(wave)
      }
     
      file.close()
      reader.close()
      
      level
      
    } catch {
      case e: FileNotFoundException => throw new Exception("Could not find the file. Sorry!")
      case e: IOException => throw new Exception("Encountered a wild IOException!")
    }
  }
  
  def loadTent: (Int, Int, Double, Double, Coordinate) = {
    
    val filename = s"$dataFolder/Tent"
    
    try {
      val file: FileReader = new FileReader(filename)
      val reader: BufferedReader = new BufferedReader(file)
      
      var currentLine: String = reader.readLine()     
      var lineStorage = Buffer[String](currentLine + "\n")
      
      while(currentLine != null){
        currentLine = reader.readLine()
        lineStorage += currentLine + "\n"
      }
      
      var cleanedStorage: String = ""
      if(!lineStorage.head.contains("-")){
        cleanedStorage = lineStorage.dropRight(1).drop(1).mkString
      } else {
        cleanedStorage = lineStorage.mkString
      }
      val infoSets: Array[Array[String]] = cleanedStorage.split("-").drop(1).map(_.split("\n"))
      
      var tentQuota = 4
      var tentPrice = 20
      var sherpaHealth = 100.0
      var sherpaDamage = 20.0
      var sherpaSpeed = Coordinate(1.3, 1.3)
      
      for(set <- infoSets) {
        
        val setHeader = set(0).toLowerCase.trim.replaceAll("\\s","")

        setHeader match {
          
          case "setup" => {
            
            for(information <- set){
              
               val infoArray = information.split(":").map(_.toLowerCase.trim)
               val infoHeader = infoArray(0) 
               
               if(infoArray.size > 1 && !infoArray(1).isEmpty()){
               
                 try {
                   
                   infoHeader match {
                     
                     case "quota" => {
                       
                       tentQuota = infoArray(1).toInt
                     }
                     
                     case "price" => {
                       
                       tentPrice = infoArray(1).toInt
                     }
                     
                     case _ => {
  
                     }
                   }
                   
                 } catch {
                   case e: Throwable => 
                 }
               }
               
            }
          }
          case "sherpasettings" => {
            
            for(information <- set){
              
               val infoArray = information.split(":").map(_.toLowerCase.trim)
               val infoHeader = infoArray(0) 
               
               if(infoArray.size > 1 && !infoArray(1).isEmpty()){
               
                 try {
                 
                   infoHeader match {
                     
                     case "health" => {
                       
                       sherpaHealth = infoArray(1).toInt
                     }
                     
                     case "damage" => {
                       
                       sherpaDamage = infoArray(1).toDouble
                     }
                     
                     case "speed" => {
                       
                       val speedCoordinate = infoArray(1).toDouble
                       sherpaSpeed = Coordinate(speedCoordinate, speedCoordinate)
                     }
                     
                     case _ => {
  
                     }
                   }
                   
                 } catch {
                   case e: Throwable => 
                 }
               }
             }
          }
          case _ => {

          }
        }
      }
      
      file.close()
      reader.close()
      
      if(tentQuota == 0 || tentQuota > 20) tentQuota = 4
      if(sherpaHealth == 0.0) sherpaHealth = 100.0
                
      (tentQuota, tentPrice, sherpaHealth, sherpaDamage, sherpaSpeed)
      
    } catch {
      case e: FileNotFoundException => throw new Exception("Could not find the file. Sorry!")
      case e: IOException => throw new Exception("Encountered a wild IOException!")
    }
  }
  
  def loadMachine: (Double, Double, Double, Int) = {
    
    val filename = s"$dataFolder/Machine"
    
    try {
      val file: FileReader = new FileReader(filename)
      val reader: BufferedReader = new BufferedReader(file)
      
      var currentLine: String = reader.readLine()
      var lineStorage = Buffer[String](currentLine + "\n")
      
      while(currentLine != null){
        currentLine = reader.readLine()
        lineStorage += currentLine + "\n"
      }
      
      var cleanedStorage: String = ""
      if(!lineStorage.head.contains("-")){
        cleanedStorage = lineStorage.dropRight(1).drop(1).mkString
      } else {
        cleanedStorage = lineStorage.mkString
      }
      val infoSets: Array[Array[String]] = cleanedStorage.split("-").drop(1).map(_.split("\n"))
      
      var machineDamage = 5.0
      var machineHitsPerMinute = 50.0
      var machineRange = 250.0
      var machinePrice = 20
      
      for(set <- infoSets) {
        
        val setHeader = set(0).toLowerCase.trim.replaceAll("\\s","")

        setHeader match {
          
          case "setup" => {
            
            for(information <- set){
              
               val infoArray = information.split(":").map(_.toLowerCase.trim)
               val infoHeader = infoArray(0) 
               
               if(infoArray.size > 1 && !infoArray(1).isEmpty()){
               
                 try {
                 
                   infoHeader match {
                     
                     case "price" => {
                       
                       machinePrice = infoArray(1).toInt
                     }
                     
                     case "range" => {
                       
                       machineRange = infoArray(1).toDouble
                     }
                     
                     case "damage" => {
                       
                       machineDamage = infoArray(1).toDouble
                     }
                     
                     case "hitsperminute" => {
                       
                       machineHitsPerMinute = infoArray(1).toDouble
                     }
                     
                     case _ => {
  
                     }
                   }
                   
                 } catch {
                   case e: Throwable => 
                 }  
               }
               
            }
          }
          case _ => {

          }
        }
      }
      
      file.close()
      reader.close()
      
      (machineDamage, machineRange, machineHitsPerMinute, machinePrice) 
      
    } catch {
      case e: FileNotFoundException => throw new Exception("Could not find the file. Sorry!")
      case e: IOException => throw new Exception("Encountered a wild IOException!")
    }    
  }
  
  def loadEnemy(typeNumber: Int): Enemy = {
    
    val filename = s"$dataFolder/Enemy $typeNumber"
    
    try {
      val file: FileReader = new FileReader(filename)
      val reader: BufferedReader = new BufferedReader(file)
      
      var currentLine: String = reader.readLine()
      var lineStorage = Buffer[String](currentLine + "\n")
      
      while(currentLine != null){
        currentLine = reader.readLine()
        lineStorage += currentLine + "\n"
      }
      
      var cleanedStorage: String = ""
      if(!lineStorage.head.contains("-")){
        cleanedStorage = lineStorage.dropRight(1).drop(1).mkString
      } else {
        cleanedStorage = lineStorage.mkString
      }
      val infoSets: Array[Array[String]] = cleanedStorage.split("-").drop(1).map(_.split("\n"))
      
      var enemyHealth = 100.0
      var enemyDamage = 2.0
      var enemySpeed = Coordinate(1.2, 1.2)
      var enemyMoney = 10
      
      for(set <- infoSets) {
        
        val setHeader = set(0).toLowerCase.trim.replaceAll("\\s","")

        setHeader match {
          
          case "setup" => {
            
            for(information <- set){
              
               val infoArray = information.split(":").map(_.toLowerCase.trim)
               val infoHeader = infoArray(0) 
               
               if(infoArray.size > 1 && !infoArray(1).isEmpty()){
               
                 try {
                 
                   infoHeader match {
                     case "health" => {
                       
                       enemyHealth = infoArray(1).toDouble
                     }
                     
                     case "damage" => {
                       
                       enemyDamage = infoArray(1).toDouble
                     }
                     
                     case "speed" => {
                       
                       val speedCoordinate = infoArray(1).toDouble
                       enemySpeed = Coordinate(speedCoordinate, speedCoordinate)
                     }
                     
                     case "money" => {
                       
                       enemyMoney = infoArray(1).toInt
                     }
                     
                     case _ => {
  
                     }
                   }
                   
                 } catch {
                   case e: Throwable => 
                 }
               }
               
            }
          }
          case _ => {

          }
        }
      }
      
      file.close()
      reader.close()
      
      if(enemyHealth > 0.0){
        new Enemy(enemyHealth, enemyDamage, enemyMoney, enemySpeed, Coordinate(0, 0))
      } else {
        new Enemy(100.0, enemyDamage, enemyMoney, enemySpeed, Coordinate(0, 0))
      }
      
    } catch {
      case e: FileNotFoundException => throw new Exception("Could not find the file. Sorry!")
      case e: IOException => throw new Exception("Encountered a wild IOException!")
    }    
  }
}