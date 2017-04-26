import scala.collection.mutable.Buffer
import processing.core._


class Player(val name: String, var hand: Buffer[Card], var collected: Buffer[Card], var score: Int, var sweeps: Int) {
  
  def scoreCount: Int = score //player's whole game score
  def spadeCount: Int = collected.filter(_.suit == 1).size //spade count for this round 
  def aceCount: Int = collected.filter(_.value == 1).size  //ace count for this round
  def cardCount: Int = collected.size  //card count for this round
  def sweepCount: Int = sweeps //sweep count for this round
  var possibilities = Buffer[(Card, Buffer[Buffer[Card]])]() //all possible combinations for player's cards
  def points = Vector[String](name, scoreCount.toString, spadeCount.toString, aceCount.toString, cardCount.toString, sweepCount.toString) //string represantions for player's different point counts
  
  def drawCard(card: Card){
     hand += card
     Deck.deck.pop      
  }
  
  def removeCard(card: Card) = {
    hand -= card
  }
  
  def takeCards(cards: Buffer[Card]) = { // collect cards from the table
    collected = collected ++ cards //add cards to collected cards
    Board.removeCards(cards)
    if(Board.cards.isEmpty) sweeps += 1 //adds sweeps if taking cards from table empties it
    var pair = (this, cards)
    if(Game.history.size >= Game.playerCount) Game.history(Game.turn) = (this, cards) else Game.history += pair //add (player, collected cards) pair to play history
  }
  
  def playCard(card: Card) = {
     Board.addCard(card)
     Game.playedCard = Some(card)
     var pair = (this, Buffer[Card](Game.playedCard.get))
     if(Game.history.size >= Game.playerCount) Game.history(Game.turn) = (this, Buffer[Card](card)) else Game.history += pair //add (player, played card) pair to play history
     Game.nextPlayer = true
  }

  def addScore = score += 1
  
  def deactivateAll = { //deactives all player's cards
    for(card <- hand){
      setDown(card)     
    	card.active = false
    }
  }
  
  def setUp(card: Card) = { //sets card on up position
    card.x = 15 *(34 + Game.players(0).hand.indexOf(card))
    card.y = 540 //up position y
  }
  
  def setDown(card: Card) = { //sets card on down position
    card.x = 15 * (34 + Game.players(0).hand.indexOf(card))
    card.y = 560 //down position y
  }
  
  
  def pointCount(pair: (Card,Buffer[Buffer[Card]])): Double = { //counts points for all possible combinations, more points the better combination
    var count = 0.0
    var together: Buffer[Card] = pair._2.flatten 
    together += pair._1
    for(i <- 14 to 16)
      count += together.count(_.valueOnHand == i) //counts all aces and 2 of spades and 10 of diamonds
    count += together.count(_.suit == 1) / 5 + together.size  //counts all spades
    count  //returns how many points the combination got
  }
  
  def makeBuffer(pair: (Card, Buffer[Buffer[Card]])): Buffer[Card] = pair._2.flatten :+ pair._1 //makes buffer from a tuple

  def chooseBest: Option[Buffer[Card]] = { //choses best combination by maximum points(most aces/spades etc.)
    if(!possibilities.isEmpty)
      Some(makeBuffer(possibilities.maxBy(pointCount)).reverse)
    else 
      None
  }
  
  def sortHand = { //sorts hand so best card to play on board is first and worst cards(aces and such) are last
    hand = this.hand.sortBy(_.valueOnHand)
    var fine     = this.hand.filter(x => x.suit != 1 && (x.valueOnHand + Board.cards.map(_.value).sum > 16) && x.valueOnHand <= 13)
    var tooSmall = this.hand.filter(x => x.suit != 1 && (x.valueOnHand + Board.cards.map(_.value).sum <= 16) && x.valueOnHand <= 13)
    var spades   = this.hand.filter(x => x.suit == 1 && x.valueOnHand < 14)
    var aces     = this.hand.filter(x => x.suit != 1 && x.valueOnHand == 14)
    var topCards = this.hand.filter(x => x.valueOnHand >= 15 || (x.valueOnHand == 14 && x.suit == 1))
    hand = fine ++ tooSmall ++ spades ++ aces ++ topCards
  }
}