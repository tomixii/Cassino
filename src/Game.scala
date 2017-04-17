import scala.collection.mutable.Buffer

object Game {
  
  val playerCount = 4
  var players = Buffer[Player]()
  var turn = 0
  
  def addPlayer(isComputer: Boolean, player: Player){
    players += player
  }
  
  def nextTurn = {
    if(!Deck.deck.isEmpty)
      players(turn).drawCard(Deck.deck(0))
    players(turn).deactivateAll
    if(turn == playerCount - 1)
      turn = 0
    else
      turn += 1
  }
  
}