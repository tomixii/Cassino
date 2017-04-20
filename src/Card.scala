import processing.core._
import scala.collection.mutable.Buffer
// suits: 0 = hearts, 1 = spades, 2 = diamonds, 3 = clubs

class Card(val value: Int, val suit: Int, val number: Int, val img: PImage) {
  var active: Boolean = false
  var isPressed = false
  var selected = false
  var clicktimer = 0
  var x = 0f
  var y = 0f
  var width = 70
  var height = 105
  
  def valueOnHand: Int = {
    if(value == 1)
      14
    else if(value == 2 && suit == 1)
      15
    else if(value == 10 && suit == 2)
      16
    else
      value
  }
  
  def checkFor = {
    var poss = Board.takeCards(Board.cards, Buffer[Card](), this.valueOnHand, this.valueOnHand)
//		println(this.valueOnHand + " = " + poss.map(_.map(_.value)))
    Game.players(Game.turn).possibilities = Game.players(Game.turn).possibilities ++ Board.findPossibilities(this, poss)
//    println(this.valueOnHand + " = " + Game.players(Game.turn).possibilities.filter(_._1 == this).map(x => (x._1.valueOnHand , x._2.map(_.map(_.value)))))
    Board.res.clear()
  }
  
  def pressForPoints: Boolean ={
//    println(this.valueOnHand + " = " + Game.players(Game.turn).possibilities.map(_._2).map(_.flatten).map(_.map(_.value)))
//    println(Game.players(Game.turn).possibilities.map(_._2).map(_.flatten))7
//    Game.players(Game.turn).possibilities.map(_._2).map(_.flatten).exists(_ == Board.cards.filter(_.selected).sortBy(_.value))
    Game.players(Game.turn).possibilities.map(x => (x._1, x._2.flatten)).exists(x => (x._1, x._2.sortBy(_.value)) == (this, Board.cards.filter(_.selected).sortBy(_.value)))
  }
  
}