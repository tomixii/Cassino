import scala.collection.mutable.Buffer
import scala.math.max

object Game {
  
  var playerCount = 4
  var players = Buffer[Player]()
  var turn = 0
  var roundOver = false
  var gameIsOn = true
  var winner: String = ""
  var dealer: Option[Player] = None
  val buttonWidth = 400
  val buttonHeight = 200
  val startStrings = Vector[String]("Continue", "New game", "Help", "Exit")
  val pauseStrings = Vector[String]("Resume", "New game", "Help", "Save and exit")
  
  def addPlayer(isComputer: Boolean, player: Player){
    players += player
  }
  
  def nextTurn = {
    
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
  
  def endOfRound = {
    Board.cards.clear()
    var spadeCounts = Buffer[(Player, Int)]()
    var cardCounts = Buffer[(Player, Int)]()
    for(player <- players){
      var spadePar = (player, player.spadeCount)
      var cardPar  = (player, player.cardCount)
      spadeCounts += spadePar
      cardCounts  += cardPar
      player.score += player.aceCount + player.cottageCount + player.collected.filter(_.valueOnHand == 15).size + player.collected.filter(_.valueOnHand == 16).size * 2    
    }
    if(spadeCounts.sortBy(_._2).apply(playerCount - 1) != spadeCounts.sortBy(_._2).apply(playerCount - 2))
      spadeCounts.maxBy(_._2)._1.score += 2
    if(cardCounts.sortBy(_._2).apply(playerCount - 1) != cardCounts.sortBy(_._2).apply(playerCount - 2))  
      cardCounts.maxBy(_._2)._1.score += 1
    var player = 0
    while(gameIsOn && player < playerCount){
      gameIsOn = players(player).score < 16
      player += 1
    }
    if(!Game.gameIsOn){
    	var scoreCounts = Buffer[(Player, Int)]()
      for(player <- Game.players){
    	  var scorePar = (player, player.score)
    	  scoreCounts += scorePar
    	}
    	winner = scoreCounts.maxBy(_._2)._1.name        
    }else   
      newRound
  }
  
  def newRound = {
    Board.cards.clear()
    for(player <- players){
      player.collected.clear()
      player.cottages = 0
    }
    Deck.shuffleDeck
    for (player <- Game.players) {
      for (i <- 0 until 3)
        player.drawCard(Deck.deck(0))
    }
    for (i <- 0 to 3){
    	Board.addCard(Deck.deck(0))
    	Deck.deck.pop      
    }
    dealer = if(players.indexOf(dealer) + 1 >= playerCount) Some(players(0)) else Some(players(players.indexOf(dealer) + 1))
    turn = if(players.indexOf(dealer) + 1 >= playerCount) 0 else players.indexOf(dealer) + 1 
  }
}