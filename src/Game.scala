import scala.collection.mutable.Buffer
import scala.math.max
import scala.collection.mutable.Queue

object Game {

  var playerCount = 4 //how many players there are in the game
  var newplayerCount = 4 //how many players there are in the next game
  var players = Buffer[Player]()
  var playedCard: Option[Card] = None
  var turn = 0 //which player's turn it is, 0 for user 
  var changeCounter = 0 //timer for slowing round changes
  var nextPlayer = false
  var roundOver = false
  var gameIsOn = true
  var changingRound = false
  var state = State.STARTMENU //state of the game(more at class State)
  var lastState = State.STARTMENU //help screen's back button knows where to go
  var winner: String = ""
  var dealer: Option[Player] = None //which player is the dealer
  var history = Buffer[(Player, Buffer[Card])]() //players' play history
  val buttonWidth = 400 //menubutton dimensions
  val buttonHeight = 200
  val startStrings = Vector[String]("Load game", "New game", "Help", "Exit") //strings for main menu 
  val pauseStrings = Vector[String]("Resume", "Main menu", "Help", "Save and exit") //and pause menu

  def addPlayer(player: Player) {
    players += player
  }

  def nextTurn = { //when player has done his/her turn this method is called
    nextPlayer = false
    players(turn).deactivateAll //makes sure all user's cards are deactivated
    players(turn).possibilities.clear() //clears player's old collect possibilities
    if (turn == playerCount - 1) //next player's turn
      turn = 0
    else
      turn += 1
  }

  def endOfRound = { //clears some buffer before next round starts and counts points for each player
    Board.cards.clear()
    history.clear
    nextPlayer = false
    var spadeCounts = Buffer[(Player, Int)]() //how many spades each player has as tuple (Player, amount of spades)
    var cardCounts = Buffer[(Player, Int)]() //how many aces each player has as tuple (Player, amount of aces)
    for (player <- players) { //counts points for each player( 1 for each ace and sweep , 1 for 2 of spades and 2 for 10 of diamonds)
      var spadePar = (player, player.spadeCount)
      var cardPar = (player, player.cardCount)
      spadeCounts += spadePar
      cardCounts += cardPar
      player.score += player.aceCount + player.sweepCount + player.collected.filter(_.valueOnHand == 15).size + player.collected.filter(_.valueOnHand == 16).size * 2
    }
    if (spadeCounts.sortBy(_._2).apply(playerCount - 1) != spadeCounts.sortBy(_._2).apply(playerCount - 2)) //+2 points for player who has most spades if second most isn't same number
      spadeCounts.maxBy(_._2)._1.score += 2
    if (cardCounts.sortBy(_._2).apply(playerCount - 1) != cardCounts.sortBy(_._2).apply(playerCount - 2)) //+1 point for player who has most cards if second most isn't same number
      cardCounts.maxBy(_._2)._1.score += 1
    var player = 0
    while (gameIsOn && player < playerCount) { //check is some player has over 16 points and ends game if does
      gameIsOn = players(player).score < 16
      player += 1
    }
    if (!Game.gameIsOn) {
      var scoreCounts = Buffer[(Player, Int)]()
      for (player <- Game.players) {
        var scorePar = (player, player.score)
        scoreCounts += scorePar
      }
      winner = scoreCounts.maxBy(_._2)._1.name //updates winner's name if game is over  
    } else
      newRound
  }

  def newRound = {
    changingRound = true //game is in the middle of changing rounds
    for (player <- players) { //clear last round points
      player.collected.clear()
      player.sweeps = 0
    }
    Deck.shuffleDeck
    dealer = if (players.indexOf(dealer.get) + 1 >= playerCount) Some(players(0)) else Some(players(players.indexOf(dealer.get) + 1)) //next dealer /clock-wise)
    turn = if (players.indexOf(dealer.get) + 1 >= playerCount) 0 else players.indexOf(dealer.get) + 1 //player next from dealer starts (clock-wise)
    for (i <- 0 until playerCount) { //creates empty history
      var pair = (players(i), Buffer[Card]())
      history += pair
    }
  }

  def changeRound = { //slows down changing of round
    if (changeCounter % 60 == 0) {
      for (i <- 0 until Game.players.size) { //deals cards one by one for each player and board
        Game.players(i).drawCard(Deck.deck(0))
        if (i == 0) Game.players(i).setDown(Game.players(i).hand.last)
      }
      Board.addCard(Deck.deck(0))
      Deck.deck.pop
    }
    if (changeCounter == 239) { // changing round takes about 4 seconds
      changeCounter = 0
      Game.changingRound = false
    } else
      changeCounter += 1
  }
  /* Method for starting new game. Clears all buffers for new elements,
   * refreshes player count, adds player and CPUs in the game and makes the user 
   * as starting player.
   */
  def newGame = {
    Game.players.clear
    Deck.deck.clear
    Board.cards.clear
    Game.history.clear
    Deck.shuffleDeck
    Game.playerCount = Game.newplayerCount
    Game.addPlayer(new Player("Player", Buffer[Card](), Buffer[Card](), 0, 0)) //adds user to the game
    for (i <- 0 until Game.playerCount - 1)
      Game.addPlayer(new Player("CPU " + (i + 1), Buffer[Card](), Buffer[Card](), 0, 0)) //adds CPUs to the game
    for (player <- Game.players) { //deal cards for players
      for (i <- 0 until 4)
        player.drawCard(Deck.deck(0))
    }
    for (i <- 0 until 4) { //deal cards on board
      Board.addCard(Deck.deck(0))
      Deck.deck.pop
    }
    for (i <- 0 until playerCount) {
      var pair = (players(i), Buffer[Card]())
      history += pair
    }

    Game.dealer = Some(Game.players.last) //define dealer
    Game.turn = 0 //and starting player as the next from dealer(clock-wise) 
    Game.changeCounter = 0
    Game.changingRound = false //refresh booleans and state
    Game.nextPlayer = false
    state = State.GAME
    Game.gameIsOn = true
  }
}