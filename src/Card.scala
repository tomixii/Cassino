import processing.core._
import scala.collection.mutable.Buffer

// suits: 0 = hearts, 1 = spades, 2 = diamonds, 3 = clubs
class Card(val value: Int, val suit: Int, val number: Int, val img: PImage) { 
  
  var active: Boolean = false   //mouse hovers on card 
  var isPressed: Boolean = false//mouse hovers and is pressed at the same time
  var selected: Boolean = false //card is being selected at board
  var x = 0f    //card coordinates
  var y = 0f
  val width = 70 // card dimensions
  val height = 105
  
  def valueOnHand: Int = { //get value of card in hand(aces = 14, 2 of spades = 15 and 10 of diamonds = 16)
    if(value == 1)
      14
    else if(value == 2 && suit == 1)
      15
    else if(value == 10 && suit == 2)
      16
    else
      value
  }
  
  def checkForCards = {
    var poss = Board.takeCards(Board.cards, Buffer[Card](), this.valueOnHand, this.valueOnHand) // all the possible combinations for the card
    Game.players(Game.turn).possibilities = Game.players(Game.turn).possibilities ++ Board.findPossibilities(this, poss) //possible combinations where same card appears only once
    Board.res.clear()
  }
  
  //checks if all selected cards exists in one of the possible combinations for the card
  def pressForPoints: Boolean = {
    Game.players(Game.turn).possibilities.map(x => (x._1, x._2.flatten)).exists(x => (x._1, x._2.sortBy(_.value)) == (this, Board.cards.filter(_.selected).sortBy(_.value)))
  }
  
  
}