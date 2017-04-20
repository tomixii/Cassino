import scala.collection.mutable.Buffer

object Game {
  
  val playerCount = 4
  var players = Buffer[Player]()
  var turn = 0
  var bestCardsToPlay = Buffer[Card]()
  
  def addPlayer(isComputer: Boolean, player: Player){
    players += player
  }
  
  def nextTurn = {
    if(!Deck.deck.isEmpty)
      players(turn).drawCard(Deck.deck(0))
//    println(Game.players(Game.turn) + ": " + Game.players(Game.turn).hand.map(_.valueOnHand))  
    players(turn).deactivateAll
    Game.players(Game.turn).possibilities.clear()
    if(turn == playerCount - 1)
      turn = 0
    else
      turn += 1
    players(turn).loop = 0  
//    for(player <- Game.players){ 
//      for(card <- player.hand){
//        var poss = Board.takeCards(Board.cards, Buffer[Card](), card.valueOnHand, card.valueOnHand)
//        Game.players(Game.turn).possibilities = Game.players(Game.turn).possibilities ++ Board.findPossibilities(card, poss) 
//        Board.res.clear()
//      }
//    }  
  }
  
  
  
}