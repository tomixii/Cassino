import scala.collection.mutable.Buffer

object Game {
  
  var players = Buffer[Player]()
  
  def addPlayer(isComputer: Boolean, player: Player){
    players += player
  }
  
  
  
}