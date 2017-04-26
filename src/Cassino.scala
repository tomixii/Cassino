import processing.core._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.math._
import java.awt.event.KeyEvent._
import java.io._
import scala.io.Source

class Cassino extends PApplet {

  var mouse = new Mouse(this)
  var debugcounter = 0        //only for debug purposes
  var computerCounter = 0     //timer for slowing CPU players' turns 
  var debug = true            //only for debug purposes
  

  override def setup() = {
    loadImages              //load all the images for cards
  }
  
  override def settings() = {
    size(1140, 680)
  }
  
  //	draw method takes care of everything that updates in game 
  override def draw() = {
    background(0, 120, 0) //set green background
    if (Game.state == State.GAME) { 
      if (Game.gameIsOn) {
        if (!Game.changingRound) { //players can't play their cards if round is changing
          updateCards
        }else{
          Game.changeRound
        }
        updateHand                
        renderCards              //renders all cards in players' hands and on board
        noFill
        playHistory              //shows playhistory on the left side on frame
        if (Scoreboard.isPressed || Game.changingRound) {
          showScores             //show scoreboard
        }
        if (!Deck.deck.isEmpty && Game.players(Game.turn).hand.size < 4 && !Game.changingRound) //draws a card if hand has less than 4 cards and deck isn't empty
          Game.players(Game.turn).drawCard(Deck.deck(0))
        if (Game.players.forall(_.hand.isEmpty) && Game.gameIsOn) {    //if all hands are empty, ends round
          Game.endOfRound
        } else if (Game.nextPlayer) { //if a player has done his/her turn, changes turn
          Game.nextTurn
        }
      } else {
        if (Game.changeCounter < 30) { //makes game winner text flashing
          fill(255, 0, 0)
          textSize(50)
          text("Game over! Winner is " + Game.winner, 180, 80)
        }
        showScores //shows scoreboard at the end of the game
        if (Game.changeCounter == 60) Game.changeCounter = 0 else Game.changeCounter += 1
      }
      strokeWeight(2)
      stroke(0,0,0)
      color("grey")          //cahnge fillcolor to grey
      rect(950, 10, 180, 80) //menu button
      color("black")         //change fillcolor to black
      textSize(40)    
      text("Menu", 990, 65)  //text inside menu button
    } else if (Game.state == State.STARTMENU || Game.state == State.PAUSEMENU) { //show menu
      showMenu
    } else if (Game.state == State.HELP) {  //show help screen
      showHelp
    }
    /*
     * ONLY FOR DEBUGGING
     */
    if (debug) {
    	fill(255, 0, 0)
    	textSize(24)
    	text("X: " + mouseX + "\n" + "Y: " + mouseY, mouseX + 20, mouseY)
      if(debugcounter == 60){
      //          for (player <- Game.players)
      //            println(Game.players.indexOf(player) + ": " + player.collected.map(_.valueOnHand))
      //          println("turn: " + Game.turn)
      //          println("player hand size: " + Game.players(0).hand.size)
      //          println("board selected sum: " + Board.cards.filter(_.selected).map(_.value).sum)
      //          println(Board.cards.filter(_.selected).map(_.value))
      }   
    }
    if (debugcounter == 61) debugcounter = 0
    debugcounter += 1

  }

  // renders cards 
  
  def renderCards = {
    for (card <- Board.cards) { //updates board cards coordinates and draws them
      card.x = Board.x + (Board.cards.indexOf(card)) % 4 * (card.width + 7)
      card.y = Board.y + (Board.cards.indexOf(card)) / 4 * (card.height + 7)
      if (card.selected) { //if card is selected, draws red rectangle around it
        strokeWeight(5)
        stroke(255, 0, 0)
        rect(card.x, card.y, card.width, card.height)
      }
      image(card.img, card.x, card.y, card.width, card.height)
    }
    for (card <- Game.players(0).hand.sortBy(_.active)) { //draws users cards, so that the active one is on top
      if (card.pressForPoints) {                          //if user can take selected cards with this card, draws red rectangle around it
        rect(card.x, card.y, card.width, card.height)
      }
      image(card.img, card.x, card.y, card.width, card.height)
    }
    for (player <- 1 until Game.playerCount) {            //updates and draws CPUs cards evenly around the board
      for (card <- Game.players(player).hand) {
        card.x = (Math.cos((player * (360 / Game.playerCount) + 90).toRadians)).toFloat - 15 * (4 - Game.players(player).hand.indexOf(card))
        card.y = (Math.sin((player * (360 / Game.playerCount) + 90).toRadians)).toFloat + 220

        pushMatrix()
        translate(width / 2, height / 2)
        rotate((player * (360 / Game.playerCount)).toRadians.toFloat)
        image(Deck.cardback, card.x, card.y, card.width, card.height)
        popMatrix()
      }
    }
  }

  def updateCards = {
    if (Game.turn == 0) { //if it's user's turn
      for (card <- Game.players(0).hand) {     //for every card
        card.active = mouse.hover(card.x, card.y, card.width, card.height) &&                    //card is active if mouse hovers on it,
          !Game.players(0).hand.drop(Game.players(0).hand.indexOf(card) + 1).exists(_.active) && //there's no other active cards on top of it,
          !Game.players(0).hand.filterNot(_ == card).exists(_.isPressed)                         //and no other cards is being dragged 
        card.checkForCards    //updates possible cards player can take from  board
        if ((card.active || card.isPressed)) { 
          if (mousePressed) {
            if (card.pressForPoints) {     //user can click a card and collect cards from board 
              Game.playedCard = Some(card) //card clicked is played(more at object Game)
              var takenCards = (Board.cards.filter(_.selected) :+ Game.playedCard.get).reverse //cards to collect
              Game.players(Game.turn).takeCards(takenCards)     //collect selected cards
              Game.nextPlayer = true                            //ends turn
            } else {
              card.x = mouseX - card.width / 2          //card is being dragged so card's coordinates equals mouse's coordinates
              card.y = mouseY - card.height / 2
              card.isPressed = true
            }
          } else {
            if (mouse.hover(Board.x, Board.y, Board.width, Board.height)) { //card has been dragged on board and released
              Game.players(Game.turn).playCard(card)
            }
            Game.players(0).setUp(card) //card lifts up 20 pixels from the other cards if mouse hovers on it
            card.isPressed = false
          }
        } else {
          Game.players(0).setDown(card) //if no card interactions, it's on its normal position
        }
      }
    }
    if (Game.turn > 0) { //if it's CPU's turn
      if (computerCounter == 60) { //slows down CPU turns
        Game.players(Game.turn).sortHand  //sorting CPU hand at this point makes it easier on other parts 
        for (card <- Game.players(Game.turn).hand) card.checkForCards  //check which cards can be taken from board with CPU's handcards
        var cards = Game.players(Game.turn).chooseBest    //chooses the best combination of cards
        if (cards.isDefined) {    //making sure not to get None.get
          Game.playedCard = Some(cards.get.head) //updates which card has been played
          Game.players(Game.turn).takeCards(cards.get) //collects cards from the board
        } else {
          Game.players(Game.turn).playCard(Game.players(Game.turn).hand.head) //plays a card
        }
        Game.nextPlayer = true

      }
      if (computerCounter == 120) computerCounter = 0 else computerCounter += 1
    }
  }

  def updateHand = { //removes played card from player's hand
    if (Game.playedCard.isDefined) {
      Game.players(Game.turn).removeCard(Game.playedCard.get)
      Game.playedCard = None
    }
  }

  def showScores = { //shows scoreboard (TAB)
    color("grey")    //change color to grey
    rect(Scoreboard.x, Scoreboard.y, Scoreboard.width, Scoreboard.height)
    stroke(0, 0, 0)  
    color("black")   //change color to black
    textSize(24)
    for (i <- 1 to Game.playerCount + 1) {
      line(Scoreboard.x + 20, Scoreboard.y + i * 40, Scoreboard.x + Scoreboard.width - 20, Scoreboard.y + i * 40) //draws horizontal lines
    }
    for (i <- 1 to 5) {
      line(Scoreboard.x + i * 150, Scoreboard.y + 20, Scoreboard.x + i * 150, Scoreboard.y + +Scoreboard.height - 20) //draws verical lines
    }
    for (i <- 0 until 5) {
      text(Scoreboard.pointStrings(i), Scoreboard.x + (i + 1) * 150 + 5, Scoreboard.y + 30) //adds headers for columns
    }

    for (i <- 0 until Game.playerCount) {
      for (j <- 0 until 6)
        text(Game.players(i).points(j), Scoreboard.x + 150 * j + 25, Scoreboard.y + (i + 1) * 40 + 30) //adds players' scores to the scoreboard
    }
  }

  def playHistory = { //shows boxes on the left on frame 
    for (pair <- Game.history) {
      var i = Game.players.indexOf(pair._1)
    	var action = if (pair._2.size == 0) "" else if (pair._2.size == 1) "played" else "took"
      color("grey") //changes color to grey
      stroke(0,0,0)
      strokeWeight(2)
      rect(10, 10 + i * 65, 60, 60) //draws boxes
      textSize(16)
      color("black") //changes color to black
      text(pair._1.name + "\n" + action, 15, 30 + i * 65) //adds text to boxes
      if (mouse.hover(10, 10 + i * 65, 60, 60))  //if mouse hovers on box, shows that player's last turn
        showHistory(pair)
    }
  }

  def showHistory(pair: (Player, Buffer[Card])) = {
    if (pair._2.size != 0){  //player must have played a turn on this round
    	color("grey")
    	rect(100, 10, pair._2.size * 80 + 10, 125) //draws new box 
    	for (j <- 0 until pair._2.size)
    		image(pair._2(j).img, 110 + 80 * j, 20, pair._2(j).width, pair._2(j).height)     //draws played or collected card(s) inside the box 
    }
  }

  def showMenu = {  // draws 4 boxes on menuscreen and text inside them
    for (i <- 0 until 4) {  
      color("grey")
      rect(Scoreboard.x + ((Game.buttonWidth + 140) * (i % 2)), Scoreboard.y + ((Game.buttonHeight + 80) * (i / 2)), Game.buttonWidth, Game.buttonHeight) //boxes
      color("black")
      textSize(40)
      if (Game.state == State.STARTMENU) { // and text. Different text for main menu and pause menu
        text(Game.startStrings(i), Scoreboard.x + ((Game.buttonWidth + 140) * (i % 2)) + Game.buttonWidth / 2 - 10 * Game.startStrings(i).length(), Scoreboard.y + ((Game.buttonHeight + 80) * (i / 2)) + Game.buttonHeight / 2 + 15)
      } else if (Game.state == State.PAUSEMENU) { //text is kind of centered in boxes
        text(Game.pauseStrings(i), Scoreboard.x + ((Game.buttonWidth + 140) * (i % 2)) + Game.buttonWidth / 2 - 10 * Game.pauseStrings(i).length(), Scoreboard.y + ((Game.buttonHeight + 80) * (i / 2)) + Game.buttonHeight / 2 + 15)
      }
    }
  }

  def showHelp = { //shows help screen, where user can find tips, controls and choose how many CPU's he/she wants

    color("grey")
    rect(10, 10, 180, 80)
    rect(10, 100, 1120, 570)
    strokeWeight(5)
    for (i <- 1 to 9) { //draws boxes for choosing number of CPUs
      if (Game.newplayerCount == i + 1) stroke(255, 0, 0) else stroke(0, 0, 0)   
      rect(i * 90, 370, 60, 60)
    }
    stroke(0,0,0)
    strokeWeight(2)
    color("black")
    textSize(40)
    text("Back", 50, 65)
    for (i <- 1 to 9)
      text(i, i * 90 + 15, 415)
    textSize(20)
    text("Drag and drop card in the middle of board to play it. \n" +
      "Select cards you want to collect and then the card from your hand to collect the cards from board. \n" +
      "Left side you can see what other players have done on their turns by hovering your mouse on the grey boxes. \n \n" +
      "[P] = Pause \n" +
      "[TAB] = Scoreboard\n\n" +
      "Number of CPUs:",
      20, 130)
  }
  
  def color(color: String) = color match { //makes it easier to understand which color is changed 
    case "grey"  => fill(160, 160, 160, 230)
    case "black" => fill(0, 0, 0, 230)
    case "red"   => fill(255, 0, 0, 230)
  }

  override def mouseClicked() = { //check which button is clicked when mouse is clicked
    if (Game.state == State.STARTMENU) { //player is in main menu
      if (mouse.hover(Scoreboard.x, Scoreboard.y, Game.buttonWidth, Game.buttonHeight)) { //Load game button on help screen
        Game.players.clear // clears all buffers for the saved game
        Deck.deck.clear
        Board.cards.clear
        Game.history.clear
        var data = ""
        for (line <- Source.fromFile("Savefile.txt").getLines()) // get info from savefile
          data = data + line + "\n"
        Load.loadGame(new StringReader(data)) //loads the data
        Game.gameIsOn = true
        Game.nextPlayer = false
        Game.state = State.GAME
      } else if (mouse.hover(Scoreboard.x + ((Game.buttonWidth + 140)), Scoreboard.y, Game.buttonWidth, Game.buttonHeight)) { //New Game
        Game.newGame
      } else if (mouse.hover(Scoreboard.x, Scoreboard.y + ((Game.buttonHeight + 80)), Game.buttonWidth, Game.buttonHeight)) { //Help
        Game.state = State.HELP
      } else if (mouse.hover(Scoreboard.x + ((Game.buttonWidth + 140)), Scoreboard.y + ((Game.buttonHeight + 80)), Game.buttonWidth, Game.buttonHeight)) { //Exit
        sys.exit()
      }
    } else if (Game.state == State.PAUSEMENU) { //player is in pause menu
      if (mouse.hover(Scoreboard.x, Scoreboard.y, Game.buttonWidth, Game.buttonHeight)) { //Resume
        Game.state = State.GAME
      } else if (mouse.hover(Scoreboard.x + ((Game.buttonWidth + 140)), Scoreboard.y, Game.buttonWidth, Game.buttonHeight)) { //Main menu
        Game.state = State.STARTMENU
        Game.lastState = State.STARTMENU
      } else if (mouse.hover(Scoreboard.x, Scoreboard.y + ((Game.buttonHeight + 80)), Game.buttonWidth, Game.buttonHeight)) { //Help
        Game.state = State.HELP
      } else if (mouse.hover(Scoreboard.x + ((Game.buttonWidth + 140)), Scoreboard.y + ((Game.buttonHeight + 80)), Game.buttonWidth, Game.buttonHeight)) { //Save and exit
        Save.saveGame("Savefile.txt")
        sys.exit()
      }
    } else if (Game.state == State.GAME) { //player in game
      if (mouse.hover(950, 10, 180, 80)) { //menu button
        if (Game.gameIsOn) {
          Game.state = State.PAUSEMENU
          Game.lastState = State.PAUSEMENU //back button knows where to go back from help screen
        } else {
          Game.state = State.STARTMENU
          Game.lastState = State.STARTMENU
        }
      }
      for (card <- Board.cards) {
        if (mouse.hover(card.x, card.y, card.width, card.height)) { //card is being selected if mouse hovers on it and is clicked
          card.selected = !card.selected
        }
      }
    } else if (Game.state == State.HELP) { //player is in help screen
      if (mouse.hover(10, 10, 180, 80)) { //back button 
        Game.state = Game.lastState
      }
      for (i <- 1 to 9) {   
        if (mouse.hover(i * 90, 370, 60, 60)) //updates player count for the next game
          Game.newplayerCount = i + 1
      }
    }
  }

  override def keyPressed() = {

    if (keyCode == VK_TAB) { //TAB shows scoreboard
      Scoreboard.isPressed = true
    }

    if (key == 'p' || key == 'P') { //Pressing P, get user to pause menu
      if (Game.state == State.PAUSEMENU)
        Game.state = State.GAME
      else if (Game.state == State.GAME) {
        Game.state = State.PAUSEMENU
        Game.lastState = State.PAUSEMENU
      }
    }
  }

  override def keyReleased() = {

    if (keyCode == VK_TAB) {
      Scoreboard.isPressed = false
    }

  }

  def loadImages = { // loads all the card images 

    Deck.h01 = loadImage("ace_of_hearts.png")
    Deck.s01 = loadImage("ace_of_spades.png")
    Deck.d01 = loadImage("ace_of_diamonds.png")
    Deck.c01 = loadImage("ace_of_clubs.png")

    Deck.h02 = loadImage("2_of_hearts.png")
    Deck.s02 = loadImage("2_of_spades.png")
    Deck.d02 = loadImage("2_of_diamonds.png")
    Deck.c02 = loadImage("2_of_clubs.png")

    Deck.h03 = loadImage("3_of_hearts.png")
    Deck.s03 = loadImage("3_of_spades.png")
    Deck.d03 = loadImage("3_of_diamonds.png")
    Deck.c03 = loadImage("3_of_clubs.png")

    Deck.h04 = loadImage("4_of_hearts.png")
    Deck.s04 = loadImage("4_of_spades.png")
    Deck.d04 = loadImage("4_of_diamonds.png")
    Deck.c04 = loadImage("4_of_clubs.png")

    Deck.h05 = loadImage("5_of_hearts.png")
    Deck.s05 = loadImage("5_of_spades.png")
    Deck.d05 = loadImage("5_of_diamonds.png")
    Deck.c05 = loadImage("5_of_clubs.png")

    Deck.h06 = loadImage("6_of_hearts.png")
    Deck.s06 = loadImage("6_of_spades.png")
    Deck.d06 = loadImage("6_of_diamonds.png")
    Deck.c06 = loadImage("6_of_clubs.png")

    Deck.h07 = loadImage("7_of_hearts.png")
    Deck.s07 = loadImage("7_of_spades.png")
    Deck.d07 = loadImage("7_of_diamonds.png")
    Deck.c07 = loadImage("7_of_clubs.png")

    Deck.h08 = loadImage("8_of_hearts.png")
    Deck.s08 = loadImage("8_of_spades.png")
    Deck.d08 = loadImage("8_of_diamonds.png")
    Deck.c08 = loadImage("8_of_clubs.png")

    Deck.h09 = loadImage("9_of_hearts.png")
    Deck.s09 = loadImage("9_of_spades.png")
    Deck.d09 = loadImage("9_of_diamonds.png")
    Deck.c09 = loadImage("9_of_clubs.png")

    Deck.h10 = loadImage("10_of_hearts.png")
    Deck.s10 = loadImage("10_of_spades.png")
    Deck.d10 = loadImage("10_of_diamonds.png")
    Deck.c10 = loadImage("10_of_clubs.png")

    Deck.h11 = loadImage("jack_of_hearts.png")
    Deck.s11 = loadImage("jack_of_spades.png")
    Deck.d11 = loadImage("jack_of_diamonds.png")
    Deck.c11 = loadImage("jack_of_clubs.png")

    Deck.h12 = loadImage("queen_of_hearts.png")
    Deck.s12 = loadImage("queen_of_spades.png")
    Deck.d12 = loadImage("queen_of_diamonds.png")
    Deck.c12 = loadImage("queen_of_clubs.png")

    Deck.h13 = loadImage("king_of_hearts.png")
    Deck.s13 = loadImage("king_of_spades.png")
    Deck.d13 = loadImage("king_of_diamonds.png")
    Deck.c13 = loadImage("king_of_clubs.png")

    Deck.cardback = loadImage("cardback.png")
    //stuff for keeping cards in order
    Deck.cardImages = Map[String, PImage]("h01" -> Deck.h01, "s01" -> Deck.s01, "d01" -> Deck.d01, "c01" -> Deck.c01,
      "h02" -> Deck.h02, "s02" -> Deck.s02, "d02" -> Deck.d02, "c02" -> Deck.c02,
      "h03" -> Deck.h03, "s03" -> Deck.s03, "d03" -> Deck.d03, "c03" -> Deck.c03,
      "h04" -> Deck.h04, "s04" -> Deck.s04, "d04" -> Deck.d04, "c04" -> Deck.c04,
      "h05" -> Deck.h05, "s05" -> Deck.s05, "d05" -> Deck.d05, "c05" -> Deck.c05,
      "h06" -> Deck.h06, "s06" -> Deck.s06, "d06" -> Deck.d06, "c06" -> Deck.c06,
      "h07" -> Deck.h07, "s07" -> Deck.s07, "d07" -> Deck.d07, "c07" -> Deck.c07,
      "h08" -> Deck.h08, "s08" -> Deck.s08, "d08" -> Deck.d08, "c08" -> Deck.c08,
      "h09" -> Deck.h09, "s09" -> Deck.s09, "d09" -> Deck.d09, "c09" -> Deck.c09,
      "h10" -> Deck.h10, "s10" -> Deck.s10, "d10" -> Deck.d10, "c10" -> Deck.c10,
      "h11" -> Deck.h11, "s11" -> Deck.s11, "d11" -> Deck.d11, "c11" -> Deck.c11,
      "h12" -> Deck.h12, "s12" -> Deck.s12, "d12" -> Deck.d12, "c12" -> Deck.c12,
      "h13" -> Deck.h13, "s13" -> Deck.s13, "d13" -> Deck.d13, "c13" -> Deck.c13)

  }
}

object Cassino {
  def main(args: Array[String]) {
    PApplet.main(Array[String]("Cassino"))
  }
}