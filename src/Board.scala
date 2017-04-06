import scala.collection.mutable.Buffer

object Board {
  val x = 420
  val y = 190
  val width = 300
  val height = 300
  var cards = Buffer[Card]()
  
  
  def addCard(card: Card){
    cards += card
    Deck.deck.pop
    cards = cards.sortBy(_.value)
  }
}