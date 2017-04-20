import processing.core._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.math._
import java.awt.event.KeyEvent._

class Cassino extends PApplet {

  
  var mouse = new Mouse(this)
  var mouseHover: Boolean = false
  var debugCounter = 0
  var computerCounter = 0
  var playedCard: Option[Card] = None
  var clicked = false
  var debug = true
  var nextPlayer = false
  
  var emptyList = Buffer[Card]()
  
  override def setup() = {
    loadImages
    Deck.shuffleDeck
    Game.addPlayer(false, new User(this, "Player 1"))
    for (i <- 0 until Game.playerCount - 1)
      Game.addPlayer(true, new Computer(this, "Player " + (i + 2)))
    for (player <- Game.players) {
      for (i <- 0 until 4)
        player.drawCard(Deck.deck(0))
    }
    for (i <- 0 to 3){
    	Board.addCard(Deck.deck(0))
    	Deck.deck.pop      
    }
    
  }

  override def settings() = {
    size(1140, 680)
  }

  override def draw() = {
    background(0, 120, 0)
    updateCards
    updateHand
    renderCards
    noFill
    strokeWeight(2)
    stroke(0,0,0)
    rect(Board.x, Board.y, Board.width, Board.height)
    fill(160,160,160, 230)
    if(Scoreboard.isPressed) showScores
    fill(255, 0, 0)
    text("X: " + mouseX + "\n" + "Y: " + mouseY, mouseX + 20, mouseY)  
    if(debugCounter == 60 && debug){
//      for(player <- Game.players)
//        println(Game.players.indexOf(player) + ": " + player.collected.map(_.valueOnHand))
//      println("turn: " + Game.turn)
//      println("player hand size: " + Game.players(0).hand.size)      
//      println("board selected sum: " + Board.cards.filter(_.selected).map(_.value).sum)
//      println(Board.cards.filter(_.selected).map(_.value))
    }
    if(debugCounter == 61) debugCounter = 0
    debugCounter += 1
    if(Game.players.forall(_.hand.isEmpty)){
       Game.endOfRound
    }else if(nextPlayer){
      nextPlayer = false      
    	Game.nextTurn
    }
    if(!Game.gameIsOn){     
    	println("Game over! Winner is " + Game.winner)
    }
  }

  def renderCards = {
    for(card <- Board.cards){
    	if(card.selected){
    		noFill 	  
    		strokeWeight(5)
    		stroke(255, 0, 0)
    		rect(card.x ,card.y , card.width, card.height)
    		
    	}
    	image(card.img,card.x ,card.y , card.width, card.height)
    }
    for(card <- Game.players(0).hand.filter(!_.active)){
      if(card.pressForPoints){
    	  rect(card.x ,card.y , card.width, card.height)
      }
    	image(card.img, card.x, card.y, card.width, card.height)
    }
    for(card <- Game.players(0).hand.filter(_.active)){
    	if(card.pressForPoints){
    	  rect(card.x ,card.y , card.width, card.height)
      }
   		image(card.img, card.x, card.y, card.width, card.height)     
    }
    for(player <- 1 until Game.playerCount){
      for( card <- Game.players(player).hand){
        card.x = (Math.cos((player * (360 / Game.playerCount) + 90).toRadians)).toFloat - 60 + 15 * Game.players(player).hand.indexOf(card)
        card.y = (Math.sin((player * (360 / Game.playerCount) + 90).toRadians)).toFloat + 220
          
        pushMatrix()
        translate(width / 2, height / 2)
        rotate((player * (360 / Game.playerCount)).toRadians.toFloat)
        image(card.img, card.x, card.y, card.width, card.height)
        popMatrix()
      }
    }
//    for(player <- 0 until Game.playerCount){
//      for(card <- Game.players(player).collected){
//        image(Deck.cardback, card.x, card.y, card.width, card.height)
//      }
//    }
    fill(0, 0, 0)
    textSize(24)
    for(player <- 0 until Game.playerCount)
      text(Game.players(player).collected.size, 20, player * 30 + 40)
  }

  def updateCards = {
    for(card <- Board.cards){
      if(mouse.hover(card.x, card.y, card.width, card.height) && mousePressed && card.clicktimer == 0){
        card.selected = !card.selected
        card.clicktimer = 10
      }
      card.clicktimer = max(card.clicktimer - 1, 0)
      
    	card.x = Board.x + (Board.cards.indexOf(card)) % 4 * (card.width + 7)
			card.y = Board.y + (Board.cards.indexOf(card)) / 4 * (card.height + 7)
    }
    if(Game.turn == 0 || Game.players(0).updateOnce){
      if(Game.players(0).updateOnce)
    	  Game.players(0).updateOnce = false
      for (card <- Game.players(0).hand) {
        card.active = mouse.hover(card.x, card.y, card.width, card.height) && 
                      !Game.players(0).hand.drop(Game.players(0).hand.indexOf(card) + 1).exists(_.active) &&
                      !Game.players(0).hand.filterNot(_ == card).exists(_.isPressed)
        if(Game.players(0).loop <= 4){
          card.checkFor              
          Game.players(0).loop += 1
        }
        if ((card.active || card.isPressed)) {
          if (mousePressed) {
            if(card.pressForPoints){
              playedCard = Some(card)
//              Game.players(0).collect(playedCard.get)
//              for(taken <- (Board.cards.filter(_.selected) :+ playedCard.get))
              Game.players(0).takeCards((Board.cards.filter(_.selected) :+ playedCard.get))
              nextPlayer = true
              Game.players(0).setUp(Game.players(0).hand.last)
            }else{
            	card.x = mouseX - card.width / 2
        			card.y = mouseY - card.height / 2
        			card.isPressed = true
            }
          }else{
            if(mouse.hover(Board.x, Board.y, Board.width, Board.height)){
              Board.addCard(card)
              playedCard = Some(card)
              nextPlayer = true
              Game.players(0).setUp(card)
            }else{
          	  Game.players(0).setUp(card)
            }
            card.isPressed = false            
          }      
        } else {
          Game.players(0).setDown(card)
        }
      }
    }
    if(Game.turn > 0){
      if(/*computerCounter == 120*/true){
    	  Game.players(Game.turn).sortHand
        for (card <- Game.players(Game.turn).hand) {
          card.checkFor    
        }
        var cards = Game.players(Game.turn).chooseBest
        if(cards.isDefined){
          playedCard = Some(cards.get.last)          
        	Game.players(Game.turn).takeCards(cards.get)
        }else{
          var card = Game.players(Game.turn).hand.head
        	playedCard = Some(card)
        	Board.addCard(card)          
        }
        
        nextPlayer = true
        
      }
      if(computerCounter == 120) computerCounter = 0 else computerCounter += 1
    }  
//    for(player <- 0 until Game.playerCount){
//      for(card <- Game.players(player).collected){
//        card.x = Game.players(player).collected.indexOf(card) * 15 + 10
//        card.y = player * card.height + 10  
//      }
//    }
  }

  def updateHand = {
    if(playedCard.isDefined){
//      println("playcard")
      Game.players(Game.turn).removeCard(playedCard.get)
      playedCard = None
    }
  }
  
  def showScores = {
	  rect(Scoreboard.x, Scoreboard.y, Scoreboard.width, Scoreboard.height)
	  stroke(0,0,0, 230)
	  fill(0,0,0,230)
	  for(i <- 1 to Game.playerCount + 1){
		  line(Scoreboard.x + 20, Scoreboard.y + i * 40, Scoreboard.x + Scoreboard.width - 20, Scoreboard.y + i * 40)
	  }
	  for(i <- 1 to 5){
		  line(Scoreboard.x + i * 150 , Scoreboard.y + 20, Scoreboard.x + i * 150, Scoreboard.y + + Scoreboard.height - 20)
		}
	  for(i <- 0 until 5){
		  text(Scoreboard.pointStrings(i), Scoreboard.x + (i + 1) * 150 + 5, Scoreboard.y + 30) 	    	    
	  }
	    
	  for(i <- 0 until Game.playerCount){
	    for(j <- 0 until 6)
	      text(Game.players(i).points(j), Scoreboard.x + 150 * j + 25, Scoreboard.y + (i + 1) * 40 + 30 )
	  }
  }
  
  override def keyPressed() = {
	  
	  if(keyCode == VK_TAB){
	    Scoreboard.isPressed = true
	  }
  }  
  
  override def keyReleased() = {
	  
	  if(keyCode == VK_TAB){
	    Scoreboard.isPressed = false
	  }
  }
  
  def loadImages = {

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

    //     println(Deck.cardImages.keys)
  }

}

object Cassino {
  def main(args: Array[String]) {
    PApplet.main(Array[String]("Cassino"))
  }
}