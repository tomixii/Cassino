import scala.collection.mutable.Buffer
import processing.core._


class Player(val name: String, var hand: Buffer[Card], var collected: Buffer[Card], var score: Int, var cottages: Int) {
  
  def scoreCount: Int = score
  def spadeCount: Int = collected.filter(_.suit == 1).size  
  def aceCount: Int = collected.filter(_.value == 1).size
  def cardCount: Int = collected.size
  def cottageCount: Int = cottages
  var updateOnce = false
  var loop = 0
  var possibilities = Buffer[(Card, Buffer[Buffer[Card]])]()
  def points = Vector[String](name, scoreCount.toString, spadeCount.toString, aceCount.toString, cardCount.toString, cottageCount.toString) 
  
  def drawCard(card: Card){
     hand += card
     Deck.deck.pop      
  }
  
  def removeCard(card: Card) = {
    hand -= card
  }
  
  def takeCards(cards: Buffer[Card]) = {
    collected = collected ++ cards
    Board.removeCard(cards)
    if(Board.cards.isEmpty) cottages += 1
  }

  def addScore = score += 1
  
  def deactivateAll = {
    for(card <- hand){
      setDown(card)     
    	card.active = false
    }
  }
  
  def setUp(card: Card) = {
    card.x = 15 *(34 + Game.players(0).hand.indexOf(card))
    card.y = 540 //up position y
  }
  
  def setDown(card: Card) = {
    card.x = 15 * (34 + Game.players(0).hand.indexOf(card))
    card.y = 560 //down position y
  }
  
  
  def pointCount(pair: (Card,Buffer[Buffer[Card]])): Double = {
    var count = 0.0
    var together: Buffer[Card] = pair._2.flatten 
    together += pair._1
    for(i <- 14 to 16)
      count += together.count(_.valueOnHand == i)
    count += together.count(_.suit == 1) / 5 + together.size  
    count  
  }
  
  def makeBuffer(pair: (Card, Buffer[Buffer[Card]])): Buffer[Card] = pair._2.flatten :+ pair._1

  def chooseBest: Option[Buffer[Card]] = {
//    println("poss: " + possibilities.map(x => (x._1.valueOnHand, x._2.map(y => y.map(z => z.valueOnHand)))))
//    println("chosen cards: " + makeBuffer(possibilities.maxBy(pointCount)).map(_.valueOnHand))
    if(!possibilities.isEmpty)
      Some(makeBuffer(possibilities.maxBy(pointCount)))
    else 
      None
  }
  
  def sortHand = {
    hand = this.hand.sortBy(_.valueOnHand)
    var fine     = this.hand.filter(x => x.suit != 1 && (x.valueOnHand + Board.cards.map(_.value).sum > 16) && x.valueOnHand <= 13)
    var tooSmall = this.hand.filter(x => x.suit != 1 && (x.valueOnHand + Board.cards.map(_.value).sum <= 16) && x.valueOnHand <= 13)
    var spades   = this.hand.filter(x => x.suit == 1 && x.valueOnHand < 14)
    var aces     = this.hand.filter(x => x.suit != 1 && x.valueOnHand == 14)
    var topCards = this.hand.filter(x => x.valueOnHand >= 15 || (x.valueOnHand == 14 && x.suit == 1))
//    println(fine.map(_.valueOnHand) + "+" + tooSmall.map(_.valueOnHand) + "+" + spades.map(_.valueOnHand) + "+" + aces.map(_.valueOnHand) + "+" + topCards.map(_.valueOnHand))
    hand = fine ++ tooSmall ++ spades ++ aces ++ topCards
  }

                 
//    var diamonds10 = Buffer[Buffer[Buffer[Card]]]()
//    var spades2    = Buffer[Buffer[Buffer[Card]]]()
//    var aces       = Buffer[Buffer[Buffer[Card]]]()
//    var spades     = Buffer[Buffer[Buffer[Card]]]()
//    for(buff <- possibilities.values){
//      for(innerBuff <- buff){
//        if(innerBuff.flatten.map(_.valueOnHand).contains(16))
//          diamonds10 += innerBuff
//        if(innerBuff.flatten.map(_.valueOnHand).contains(15))
//          spades2 += innerBuff
//        if(innerBuff.flatten.map(_.valueOnHand).contains(14))
//          aces += innerBuff
//        if(innerBuff.flatten.map(_.suit).contains(1))
//          spades += innerBuff   
//      }
//    }  
//      
//    
//    if(!diamonds10.isEmpty)
//      (hand.filter(_.valueOnHand == diamonds10.maxBy(_.flatten.size).head.map(_.value).sum)(0), diamonds10.maxBy(_.flatten.size))
//    else if(possibilities.exists(_._1.valueOnHand == 16))
//      (possibilities.keySet.filter(_.valueOnHand == 16).head, possibilities(possibilities.keySet.filter(_.valueOnHand == 16).head).head)
//    else if(!spades2.isEmpty)
//      (hand.filter(_.valueOnHand == spades2.maxBy(_.flatten.size).head.map(_.value).sum)(0), spades2.maxBy(_.flatten.size))
//    else if(possibilities.exists(_._1.valueOnHand == 15))
//      (possibilities.keySet.filter(_.valueOnHand == 15).head, possibilities(possibilities.keySet.filter(_.valueOnHand == 15).head).head)
//    else if(!aces.isEmpty)
//      (hand.filter(_.valueOnHand == aces.maxBy(_.flatten.filter(_.value == 1).size).head.map(_.value).sum)(0), aces.maxBy(_.flatten.filter(_.value == 1).size))
//    else if(!spades.isEmpty)
//      (hand.filter(_.valueOnHand == spades.maxBy(_.flatten.filter(_.value == 1).size).head.map(_.value).sum)(0), spades.maxBy(_.flatten.filter(_.value == 1).size))
//    else
//      (possibilities.maxBy(_._2.maxBy(_.flatten.size).size)._1, possibilities.maxBy(_._2.maxBy(_.flatten.size).size)._2(0))
//    
//  }
  
}