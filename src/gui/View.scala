package gui

import core._
import processing.core._
import processing.core.PConstants._
import javax.swing.JFrame
import scala.collection.mutable.Buffer
import scala.util.Random

object View extends PApplet {
  
  //Variables used to configure screen width and height.
  val screenWidth = 790
  val screenHeight = 735
  
  /*
   * The pictures are first loaded and then resized to fit the screen layout.
   * The instances for the main menu buttons are created after all the image handling.
   */
  val mainMenuImage: PImage = loadImage("pictures/MainScreen.png")
  val helpTextImage: PImage = loadImage("pictures/HelpText.png")
  val baseCampImage: PImage = loadImage("pictures/BaseCamp.png")
  val campIIImage: PImage = loadImage("pictures/CampII.png")
  val theSummitImage: PImage = loadImage("pictures/TheSummit.png")
  val levelFailedImage: PImage = loadImage("pictures/LevelFailed.png") 
  val levelCompletedImage: PImage = loadImage("pictures/LevelCompleted.png")
  val dollarBagImage: PImage = loadImage("pictures/DollarBag.png")
  val snowballMachineImage: PImage = loadImage("pictures/SnoWizard.png")
  val tentImage: PImage = loadImage("pictures/SherpaTent.png")
  val sherpaImage: PImage = loadImage("pictures/Sherpa.png")
  val enemyImage: PImage = loadImage("pictures/EvilTourist.png")
  
  val level1ButtonImage: PImage = loadImage("pictures/Level1Button.png")
  val level2ButtonImage: PImage = loadImage("pictures/Level2Button.png")
  val level3ButtonImage: PImage = loadImage("pictures/Level3Button.png")
  val machineButtonImage: PImage = loadImage("pictures/MachineButton.png")
  val tentButtonImage: PImage = loadImage("pictures/TentButton.png")
  val returnButtonImage: PImage = loadImage("pictures/ReturnToMenuButton.png")
  val nextLevelButtonImage: PImage = loadImage("pictures/NextLevelButton.png")
  val tryAgainButtonImage: PImage = loadImage("pictures/TryAgainButton.png")
  val helpButtonImage: PImage = loadImage("pictures/HelpButton.png")
  val startButtonImage: PImage = loadImage("pictures/StartButton.png")
  val exitButtonImage: PImage = loadImage("pictures/ExitLevelButton.png")
  
  mainMenuImage.resize(790, 735)
  helpTextImage.resize(790, 735)
  baseCampImage.resize(790, 735)
  campIIImage.resize(790, 735)
  theSummitImage.resize(790, 735)
  levelFailedImage.resize(500,250)
  levelCompletedImage.resize(500, 250)
  dollarBagImage.resize(52, 52)
  snowballMachineImage.resize(90, 80)
  tentImage.resize(120, 80)
  sherpaImage.resize(60, 80)
  enemyImage.resize(60, 80)
  
  level1ButtonImage.resize(180, 90)
  level2ButtonImage.resize(180,90)
  level3ButtonImage.resize(180, 90)
  helpButtonImage.resize(180, 90)
  startButtonImage.resize(200, 90)
  machineButtonImage.resize(180, 80)
  tentButtonImage.resize(180, 80)
  returnButtonImage.resize(200, 90)
  nextLevelButtonImage.resize(200, 90)
  tryAgainButtonImage.resize(200, 90)
  exitButtonImage.resize(100, 60)
  
  val levelPictures: Map[Int, PImage] = Map(1 -> baseCampImage, 2 -> campIIImage, 3 -> theSummitImage)
  
  val level1Button = new LevelButton(level1ButtonImage, 65, 500, 180, 90)
  val level2Button = new LevelButton(level2ButtonImage, 350, 250, 180, 90)
  val level3Button = new LevelButton(level3ButtonImage, 304, 15, 180, 90)
  val helpButton = new LevelButton(helpButtonImage, 570, 500, 180, 90)
  val startButton = new LevelButton(startButtonImage, 300, 360, 200, 90)
  val returnButton = new LevelButton(returnButtonImage, 300, 240, 200, 90)
  val backToMenuButton = new LevelButton(returnButtonImage, 180, 450, 200, 90)
  val nextLevelButton = new LevelButton(nextLevelButtonImage,  420, 450, 200, 90)
  val tryAgainButton = new LevelButton(tryAgainButtonImage, 420, 450, 200, 90)
  val machineButton = new LevelButton(machineButtonImage, 205, 5, 180, 90)
  val tentButton = new LevelButton(tentButtonImage, 400, 5, 180, 90)
  val exitButton = new LevelButton(exitButtonImage, 5, 5, 100, 60)
  
  /*
   * Below are the main variables of the game. They mainly contain important level information or 
   * work as flags that signal the players choices to the main drawing function.
   */
  var mainMenuOn = true
  var helpTextOn = false
  var gameOn = false

  var currentLevelNumber = 1
  val dataProcessor: DataProcessor = new DataProcessor()
  var currentLevel: Level = null
  
  var choseDefenseSpot = false
  var firstPlacement = true
  var spotChosen: Coordinate = null
  
  var enemiesOnLevel: Buffer[Enemy] = Buffer()
  
  def main(args: Array[String]) {
    val frame = new JFrame("Protecting Mount Everest")
  
    frame.getContentPane.add(this)
    init
    frame.setSize(this.getSize)
    frame.pack
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)
  }
  
  // A method that configures the size of the screen and its refreshing rate.
  override def setup() {
    size(screenWidth, screenHeight)
    frameRate(50)
  }
  
  /* The main drawing function of the application.
   * It draws all the components a player can see on the screen.
   */
  override def draw(): Unit = {
    
    if(mainMenuOn){
      
      imageMode(CORNER)
      image(mainMenuImage, 0, 0)
      
      level1Button.draw
      level2Button.draw
      level3Button.draw
      helpButton.draw
      
    } else if(helpTextOn) {
      
      imageMode(CORNER)
      image(helpTextImage, 0, 0)
      
    } else {
      
      drawLevel(currentLevelNumber)
    }
  }
  
  /* Here we have a method that tracks the clicks of the mouse and sets flag variables based on
   * the click coordinates. It makes the placement of defense equipment and button navigation possible. 
   * 
   * If you wish to make your own path through the level, the following println command makes
   * coordinates appear on the Console window.
   * Feel free to copy and paste them into a Level setting file.
   * println(mouseX + ", "+ mouseY + "#")
   */
  override def mouseClicked(): Unit = {
    
    val clickedCoord = Coordinate(mouseX, mouseY)
    println(mouseX + ", "+ mouseY + "#")
    
    if(mainMenuOn){
      
      if(level1Button.mouseIsOver()){
        
        setLevel(1)
        
      } else if(level2Button.mouseIsOver()) {
        
        setLevel(2)
        
      } else if(level3Button.mouseIsOver()) {
   
        setLevel(3)
        
      } else if(helpButton.mouseIsOver()) {
        
        mainMenuOn = false
        helpTextOn = true
      }
      
    } else if(helpTextOn) {
      
      clickedCoord match {
        case _ => {
          mainMenuOn = true
          helpTextOn = false
        }
      }
      
    } else if(!mainMenuOn && choseDefenseSpot) {
      
      if(machineButton.mouseIsOver()) {
        
        setMachine(currentLevel.defenseSpots.find(coord => coord.isWithin(spotChosen, 50.0)).get)
        choseDefenseSpot = false
        
        if(firstPlacement) firstPlacement = false
        
      } else if(tentButton.mouseIsOver()){
        
        setTent(currentLevel.defenseSpots.find(coord => coord.isWithin(spotChosen, 50.0)).get)
        choseDefenseSpot = false
        
        if(firstPlacement) firstPlacement = false
        
      } else if(exitButton.mouseIsOver()){
        
        gameOn = false
        mainMenuOn = true
      } else {
        choseDefenseSpot = false
      }
      
    } else if(gameOn) {
      
      if(exitButton.mouseIsOver()){
        
        gameOn = false
        mainMenuOn = true
        
      } else if(currentLevel.occupiedSpots.exists(coord => coord.isWithin(clickedCoord, 50.0) &&
          currentLevel.machines.exists(instance => instance.position.isWithin(clickedCoord, 50)) )){
        
        currentLevel.machines.find(
          machine => machine.position.isWithin(clickedCoord, 50.0)).get.toggleRangeVisibility()
          
      } else if(currentLevel.defenseSpots.exists(coord => coord.isWithin(clickedCoord, 50.0)) 
          && currentLevel.freeSpots.exists(coord => coord.isWithin(clickedCoord, 50.0))) {      
        
        choseDefenseSpot = true
        spotChosen = clickedCoord
      }
      
    } else {
      
      if (!gameOn && !currentLevel.isFinished && startButton.mouseIsOver()) {
        
        gameOn = true
        
      } else if( ( currentLevel.isFinished && backToMenuButton.mouseIsOver ) 
          || ( !currentLevel.isFinished && !gameOn && returnButton.mouseIsOver ) ){
        
        mainMenuOn = true
      
      } else if(currentLevel.isFinished && currentLevel.waves.forall(_.isOver) && nextLevelButton.mouseIsOver && currentLevel.number < 3) {
        
        setLevel(currentLevel.number + 1)
        
      } else if(currentLevel.isFinished && tryAgainButton.mouseIsOver){
        
        setLevel(currentLevel.number)
      }
    }
  }
 
  /* 
   * This is the method that draws the level and all the components you can see on the screen
   * after you have chosen your preferred level in the main menu. 
   */
  def drawLevel(levelNumber: Int): Unit = {
    
    imageMode(CORNER)
    image(levelPictures(levelNumber), 0, 0)
    
    if(currentLevel.isFinished){
      
      imageMode(CENTER)
      
      if(currentLevel.waves.forall(_.isOver)){
      
        image(levelCompletedImage, 400, 300)
        backToMenuButton.draw()
        if(currentLevel.number < 3) nextLevelButton.draw()
        
      } else {
        
        image(levelFailedImage, 400, 300)
        backToMenuButton.draw()
        tryAgainButton.draw()
      }
      gameOn = false
      
    } else if(gameOn){
      
      imageMode(CENTER)
      image(dollarBagImage, 740, 40) 
    
      textSize(23)
      fill(0)
      text(s"${currentLevel.currentMoney}", 718, 90)
      
      exitButton.draw()
          
      val currentTime: Long = System.currentTimeMillis()
      
      if( (currentTime - currentLevel.latestWaveStarted) >= 12000
          && currentLevel.waveCounter < currentLevel.waves.size
          && !firstPlacement){
        
        setWaves()
      }
      
      if(choseDefenseSpot){
        
        machineButton.draw()
        tentButton.draw()
      }
      
      imageMode(CENTER)
      
      currentLevel.tents.foreach(tent => image(tentImage, tent.x, tent.y))

      var sherpasOnLevel = currentLevel.tents.flatMap(_.sherpas)
      
      sherpasOnLevel.filter(_.alive).foreach{
        sherpa => 
          image(sherpaImage, sherpa.x, sherpa.y)
          drawHealthBar(sherpa)
      }
      
      currentLevel.machines.foreach {
        machine => 
          image(snowballMachineImage, machine.x, machine.y)
          
          if(machine.hasRangeVisible) drawMachineRange(machine)
          
          if(machine.action){
            
            stroke(245)
            strokeWeight(5)
            line(machine.x, machine.y, machine.target.x, machine.target.y)
            
            machine.target.health -= machine.damage
            
            strokeWeight(1) //sets the stroke weight back to normal, so other borders won't be messed with
          }
      }
      
      enemiesOnLevel.filter(_.alive).foreach{
        enemy => 
          image(enemyImage, enemy.x, enemy.y)
          drawHealthBar(enemy)
      }
      
      enemiesOnLevel.filter(_.alive).foreach(_.action)
      sherpasOnLevel.filter(_.alive).foreach(_.action)
      
      enemiesOnLevel.filter(!_.alive).foreach(_.dropMoney)
      sherpasOnLevel.filter(!_.alive).foreach(_.revive)
       
    } else {
      startButton.draw()
      returnButton.draw()
    }
   
  }
  
  def drawMachineRange(machine: SnowballMachine) {

    strokeWeight(3)
    stroke(0, 0, 150, 140)
    fill(0, 0, 150, 40)
    
    ellipseMode(RADIUS)
    ellipse(machine.x, machine.y, machine.range.toFloat, machine.range.toFloat)
    
    strokeWeight(1)
  }
  
  def drawHealthBar(character: Character): Unit = {
    
    val barWidth: Float = 50
    val barHeight: Float = 5
    val barPositionX = character.x - 25
    val barPositionY = character.y - 46
    val currentHealth: Float = ( (character.health / character.maxHealth) * barWidth).toFloat
    
    fill(0, 255, 0)
    noStroke()
    rect(barPositionX, barPositionY, currentHealth, barHeight)
    
    noFill()
    stroke(0)
    rect(barPositionX, barPositionY, barWidth, barHeight)
  }
  
  /* Here below we have the methods that load the data files for every level, machine and tent.
  *  These methods update the level variables and overwrite previous game information. In a
  *  nutshell, they set up the level and enable gameplay.
  */
  def setLevel(number: Int): Unit = {
    
    currentLevelNumber = number
    currentLevel = dataProcessor.loadLevel(number)
    
    mainMenuOn = false
    firstPlacement = true
    enemiesOnLevel = Buffer()
  }
  
  def setWaves(): Unit = {
    
    currentLevel.latestWaveStarted = System.currentTimeMillis()
    currentLevel.waves(currentLevel.waveCounter).enemies.foreach(enemy => enemiesOnLevel += enemy)
    currentLevel.waveCounter += 1
  }
  
  def setMachine(position: Coordinate): Unit = {
    
    val machineInfo: (Double, Double, Double, Int) = dataProcessor.loadMachine
    val machine = new SnowballMachine(machineInfo._1, machineInfo._2, machineInfo._3, position, machineInfo._4, currentLevel)
    
    currentLevel.addMachine(machine)
  }
  
  def setTent(position: Coordinate): Unit = {
    
    val tentInfo: (Int, Int, Double, Double, Coordinate) = dataProcessor.loadTent
    val tent = new Tent(tentInfo._1, Buffer[Sherpa](), position, tentInfo._2, currentLevel)
    
    for(sherpa <- 0 until tent.quota){
      
      var startPosition = position.modify(Random.nextInt(80), Random.nextInt(80))
      val newSherpa = new Sherpa(tentInfo._3, tent, tentInfo._4, tentInfo._5, startPosition)
      
      tent.addSherpa(newSherpa)
    }
    currentLevel.addTent(tent)
  }
  
  /*
   *  A custom button class that provides methods for drawing button images and changing
   *  the tint of the image based on the location of the mouse.
   */
  class LevelButton(val buttonImage: PImage, x: Float, y: Float, width: Float, height: Float){
    
    def mouseIsOver(): Boolean = {
      
      val xIsWithin = (x + width) > mouseX && mouseX > x
      val yIsWithin = (y + height) > mouseY && mouseY > y
      
      xIsWithin && yIsWithin
    }
    
    def draw(): Unit = {
      
      imageMode(CORNER)
      
      if(this.mouseIsOver()){
        image(buttonImage, x, y)
      } else {
        tint(255, 210)
        image(buttonImage, x, y)
        tint(255, 255)
      }
    }
    
  }
}