import scala.collection.mutable.Buffer

class Player {
  
  var hand = Buffer[Card]()
  var points: Int = 0
  var score: Int = 0
  
  def drawCard(card: Card){
     hand += card
     Deck.deck.pop   
  }
  
  def discard(card: Card){
    hand -= card
  }
  
  def addPoint = points += 1
}