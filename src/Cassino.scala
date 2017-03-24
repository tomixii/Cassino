import processing.core._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap

class Cassino extends PApplet {

  val playerCount = 4
  var mouse = new Mouse(this)
  var mouseHover: Boolean = false

  override def setup() = {
    loadImages
    Deck.shuffleDeck
    Game.addPlayer(false, new User)
    for (i <- 0 until playerCount - 1)
      Game.addPlayer(true, new Computer)
    for (player <- Game.players) {
      for (a <- 0 until 4)
        player.drawCard(Deck.deck(0))
    }
    for (i <- 0 to 3)
      Board.addCard(Deck.deck(0))
  }

  override def settings() = {
    size(1140, 680)
  }

  override def draw() = {
    background(0, 120, 0)
    updateCards
    drawCards
    if(Game.players(0).hand.find(_.active).isDefined)
      whichCards
    for (card <- Board.cards){    
    	image(card.img, width / 2 + (Board.cards.indexOf(card) - 2) * (card.width + 5), height / 2 , card.width, card.height)
    }
    noFill
    rect(420, 190, 300, 300)
    fill(255, 0, 0)
    text("X: " + mouseX + "\n" + "Y: " + mouseY, mouseX + 20, mouseY)

  }
  
  def whichCards = {
    for(card <- Game.players(0).hand){
      getCards(Board.cards, Buffer[Card](), card.valueOnHand, card.valueOnHand)
    }
  }
  
  def updateCards = {
    
  }
  
  def getCards(from: Buffer[Card], to: Buffer[Card], sum: Int, initialSum: Int): Unit = {
    if(to.map(_.value).sum == initialSum){
    }else{
      for(card <- from){
        to += card        
    	  getCards(from.filter(_ != card), to, sum - card.value, initialSum)
      }
    }
      
  }

  def drawCards = {
    for (card <- Game.players(0).hand) {
      card.x = width / 2 - 55 + 15 * Game.players(0).hand.indexOf(card)
      card.y = height / 2 + 220
      mouseHover = mouse.hover(card.x, card.y, card.width, card.height, card == Game.players(0).hand(Game.players(0).hand.size - 1))
      if(mouseHover) {
        card.y -= 20
        card.active = true
      }else{        
    	  card.active = false
      }
      
      if(mouseHover && mousePressed){
        card.x = mouseX - card.width / 2
        card.y = mouseY - card.height / 2
      }
        
      image(card.img, card.x, card.y, card.width, card.height)
    }

    for (player <- 1 until Game.players.size) {
      for (card <- Game.players(player).hand) {
        card.x = (Math.cos((player * (360 / playerCount) + 90).toRadians)).toFloat - 60  + 15 * Game.players(player).hand.indexOf(card) 
        card.y = (Math.sin((player * (360 / playerCount) + 90).toRadians)).toFloat + 220
        pushMatrix()
        translate(width / 2, height / 2)
        rotate((player * (360 / playerCount)).toRadians.toFloat)
        image(Deck.cardback, card.x, card.y, card.width, card.height)
        popMatrix()

      }
    }
  }

  def loadImages {

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