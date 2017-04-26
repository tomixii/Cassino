import java.io._
import scala.collection.mutable.Buffer

object Save {
  
  def saveGame(fileName: String)  = {
    val writer = new PrintWriter(new File(fileName))
    
    for(i <- 0 until Game.playerCount){ //write all players' infos in file
    	writer.write("#" + Game.players(i).name + "\n")
    	writer.write("hand: " + Game.players(i).hand.map(makeString).mkString(",") + "\n")
    	writer.write("collected: " + Game.players(i).collected.map(makeString).mkString(",") + "\n")
    	writer.write("score: " + Game.players(i).score + "\n")
    	writer.write("sweeps: " + Game.players(i).sweeps + "\n")
    }
    
    writer.write("#GAME\n") // write game's info in file
    writer.write("board: " + Board.cards.map(makeString).mkString(",") + "\n") 
    writer.write("deck: " + Deck.deck.map(makeString).mkString(",") + "\n")
    writer.write("history: " + Game.history.map(x => x._1.name + ";" + x._2.map(makeString).mkString("-")).mkString(",") + "\n")
    writer.write("turn: " + Game.turn + "\n")
    writer.write("dealer: " + Game.players.indexOf(Game.dealer.get) + "\n")
    writer.write("playerCount: " + Game.playerCount + "\n")
    
    writer.close
  }
  
  def makeString(card: Card): String = { //makes string representation from card
    
    def getSuitChar(i: Int): Char = i match { //get card's suit (0 = hearts, 1 = spades, 2 = diamonds, 3 = clubs)
      case 0 => 'h'
      case 1 => 's'
      case 2 => 'd'
      case 3 => 'c'  
    }
    
    def getValueString(i: Int): String = { //get card value
      if(i < 10)
        "0" + i.toString()
      else
        i.toString
    }
    
    getSuitChar(card.suit) + getValueString(card.value) //combine cards suit and value to make a string
  }
}