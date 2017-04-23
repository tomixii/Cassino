import java.io.BufferedReader
import java.io.IOException
import java.io.Reader
import scala.collection.mutable.Buffer

object Load {

  def loadGame(input: Reader) = {
    val lineReader = new BufferedReader(input)
    var currentLine = lineReader.readLine()
    val headerParts = currentLine.split(" ")
    var text = Buffer[String]()
    var line = 0
    var fileRead = false
    var infoRead = false
    var playersRead = 0

    def isValidLine: Boolean = {
      if (!fileRead) {
        !text(line).startsWith("#")
      } else {
        false
      }
    }

    while (currentLine != null) {
      text += currentLine
      currentLine = lineReader.readLine()
    }

    while (line < text.size) {
      if (text(line).toUpperCase().startsWith("#PLAYER".toUpperCase())) {
        readPlayerInfo
        playersRead += 1
      } else if (text(line).toUpperCase() == "#GAME".toUpperCase()) {
        readGameInfo
        infoRead = true
      }
      if(line + 1 < text.size)
        if (!text(line).startsWith("#")) 
          line = math.min(line + 1, text.size)
    }

    def getValue(s: String): Int = s.toInt

    def getSuit(c: Char): Int = c match {
      case 'h' => 0
      case 's' => 1
      case 'd' => 2
      case 'c' => 3
    }

    def getNumber(s: String): Int = 4 * (getValue(s.tail) - 1) + getSuit(s.head)

    def getImage(s: String) = Deck.cardImages(Deck.cardStrings(getNumber(s)))

    def readPlayerInfo = {
      var hand = Buffer[Card]()
      var collected = Buffer[Card]()
      var score = 0
      var cottages = 0
      if (line < text.size) {
        do {
          line = math.min(line + 1, text.size)
          println(text(line))
          if (text(line).toLowerCase().startsWith("hand".toLowerCase())) {
            var cards = text(line).split(":")(1).trim.split(",")
            for (s <- cards) {
              if (!s.isEmpty) {
                var card = new Card(getValue(s.tail), getSuit(s.head), getNumber(s), getImage(s))
                hand += card
              }
            }
          } else if (text(line).toLowerCase().startsWith("collected".toLowerCase())) {
            var cards = text(line).split(":")(1).trim.split(",")
            //        		println(text(line))
            for (s <- cards) {
              if (!s.isEmpty) {
                //                println(playersRead + ": " + cards.mkString(","))
                var card = new Card(getValue(s.tail), getSuit(s.head), getNumber(s), getImage(s))
                collected += card
              }
            }
          } else if (text(line).toLowerCase().startsWith("score".toLowerCase())) {
            score = text(line).split(":")(1).trim.toInt
          } else if (text(line).toLowerCase().startsWith("cottages".toLowerCase())) {
            cottages = text(line).split(":")(1).trim.toInt
          }
        } while (!text(line).startsWith("#"))
      }
      println(hand.map(_.value))
      Game.players += new Player("Player " + (playersRead + 1), hand, collected, score, cottages)
    }

    def readGameInfo = {
      if (line < text.size) {
        do {
          println(text(line))
          if (text(line).toLowerCase().startsWith("board".toLowerCase())) {
            var cards = text(line).split(":")(1).trim.split(",")
            for (s <- cards) {
              if (!s.isEmpty) {
                var card = new Card(getValue(s.tail), getSuit(s.head), getNumber(s), getImage(s))
                Board.addCard(card)
              }
            }
          } else if (text(line).toLowerCase().startsWith("deck".toLowerCase())) {
            var cards = text(line).split(":")(1).trim.split(",")
            for (s <- cards) {
              if (!s.isEmpty) {
                var card = new Card(getValue(s.tail), getSuit(s.head), getNumber(s), getImage(s))
                Deck.addCard(card)
              }
            }
          } else if (text(line).toLowerCase().startsWith("turn".toLowerCase())) {
            Game.turn = text(line).split(":")(1).trim.toInt
          } else if (text(line).toLowerCase().startsWith("dealer".toLowerCase())) {
            Game.dealer = Some(Game.players(text(line).split(":")(1).trim.toInt))
          } else if (text(line).toLowerCase().startsWith("playerCount".toLowerCase())) {
            Game.playerCount = text(line).split(":")(1).trim.toInt
          }
          line = math.min(line + 1, text.size)
    		  fileRead = text.size == line
        } while (isValidLine)
      }
      
    }
  }
}