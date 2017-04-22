import java.io._
import scala.collection.mutable.Buffer

object Save {
  
  def saveGame(fileName: String)  = {
    val writer = new PrintWriter(new File(fileName))
    for(i <- 0 until Game.playerCount){
    	writer.write("#" + Game.players(i).name + "\n")
    	writer.write("hand: " + Game.players(i).hand.map(makeString).mkString(",") + "\n")
    	writer.write("collected: " + Game.players(i).collected.map(makeString).mkString(",") + "\n")
    	writer.write("score: " + Game.players(i).score + "\n")
    	writer.write("cottages: " + Game.players(i).cottages + "\n")
    }
    writer.write("#GAME\n")
    writer.write("board: " + Board.cards.map(makeString).mkString(",") + "\n") 
    writer.write("deck: " + Deck.deck.map(makeString).mkString(",") + "\n")
    writer.write("turn: " + Game.turn + "\n")
    writer.write("dealer: " + Game.players.indexOf(Game.dealer.get) + "\n")
    writer.write("playerCount: " + Game.playerCount + "\n")
//    writer.write("#END")
    writer.close
  }
  
  def makeString(card: Card): String = {
    
    def getSuitChar(i: Int): Char = i match {
      case 0 => 'h'
      case 1 => 's'
      case 2 => 'd'
      case 3 => 'c'  
    }
    
    def getValueString(i: Int): String = {
      if(i < 10)
        "0" + i.toString()
      else
        i.toString
    }
    
    getSuitChar(card.suit) + getValueString(card.value)
  }
}