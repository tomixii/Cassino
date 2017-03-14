import scala.collection.mutable.Buffer

object Board {
  val x = 10
   var cards = Buffer[Card]()
  
  def addCard(card: Card){
    cards += card
    Deck.deck -= card
//    cards = cards.sortBy(_.value)
  }
}