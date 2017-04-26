import java.io.BufferedReader
import java.io.IOException
import java.io.Reader
import scala.collection.mutable.Buffer

object Load {

  def loadGame(input: Reader) = { //loads a game from file
    val lineReader = new BufferedReader(input)
    var currentLine = lineReader.readLine()
    var text = Buffer[String]() //file in bufferform
    var line = 0 //starting line
    var fileRead = false
    var playersRead = 0 //how many player have been read already
    var playerName = ""

    def isValidLine: Boolean = {
      if (!fileRead) {
        !text(line).startsWith("#")
      } else {
        false
      }
    }

    while (currentLine != null) { //adds all the lines to buffer
      text += currentLine
      currentLine = lineReader.readLine()
    }

    while (line < text.size) { // while there's still something to read
      if (text(line).toUpperCase().startsWith("#PLAYER".toUpperCase()) || text(line).toUpperCase().startsWith("#CPU".toUpperCase())) { //if there's new player or cpu
        playerName = text(line).trim.tail //name is line minus '#'
        readPlayerInfo
        playersRead += 1
      } else if (text(line).toUpperCase() == "#GAME".toUpperCase()) { //load game variables and buffers elements
        readGameInfo
      }
      if (line + 1 < text.size)
        if (!text(line).startsWith("#")) //go to next line 
          line = math.min(line + 1, text.size)
    }

    def getValue(s: String): Int = s.toInt //get value from card string

    def getSuit(c: Char): Int = c match { //get suit from card string (0 = hearts, 1 = spades, 2 = diamonds, 3 = clubs)
      case 'h' => 0
      case 's' => 1
      case 'd' => 2
      case 'c' => 3
    }

    def getNumber(s: String): Int = 4 * (getValue(s.tail) - 1) + getSuit(s.head) //get card number from card string

    def getImage(s: String) = Deck.cardImages(Deck.cardStrings(getNumber(s))) //get image from card string

    def makeCard(str: String): Card = new Card(getValue(str.tail), getSuit(str.head), getNumber(str), getImage(str)) //make card from the info card string gives

    def readPlayerInfo = {
      var hand = Buffer[Card]()
      var collected = Buffer[Card]()
      var score = 0
      var sweeps = 0
      if (line < text.size) {
        do { //read at least one line
          line = math.min(line + 1, text.size) // next line
          if (text(line).toLowerCase().startsWith("hand".toLowerCase())) { //hand info
            var cards = text(line).split(":")(1).trim.split(",")
            for (s <- cards) {
              if (!s.isEmpty) {
                var card = new Card(getValue(s.tail), getSuit(s.head), getNumber(s), getImage(s))
                hand += card
              }
            }
          } else if (text(line).toLowerCase().startsWith("collected".toLowerCase())) { //collected cards info
            var cards = text(line).split(":")(1).trim.split(",")
            for (s <- cards) {
              if (!s.isEmpty) {
                var card = new Card(getValue(s.tail), getSuit(s.head), getNumber(s), getImage(s))
                collected += card
              }
            }
          } else if (text(line).toLowerCase().startsWith("score".toLowerCase())) { //score info
            score = text(line).split(":")(1).trim.toInt
          } else if (text(line).toLowerCase().startsWith("sweeps".toLowerCase())) { //sweep info
            sweeps = text(line).split(":")(1).trim.toInt
          }
        } while (!text(line).startsWith("#"))
      }
      Game.players += new Player(playerName, hand, collected, score, sweeps) //create new player with given infos
    }

    def readGameInfo = {
      if (line < text.size) {
        do {
          if (text(line).toLowerCase().startsWith("board".toLowerCase())) { //board cards info
            if (!text(line).split(":")(1).trim.isEmpty)
              Board.cards = text(line).split(":")(1).trim.split(",").map(makeCard).toBuffer
          } else if (text(line).toLowerCase().startsWith("deck".toLowerCase())) { //deck info
              var cards = text(line).split(":")(1).trim.split(",")
              for (s <- cards) {
                if (!s.isEmpty) {
                  var card = new Card(getValue(s.tail), getSuit(s.head), getNumber(s), getImage(s))
                  Deck.addCard(card)
                }
              }
          } else if (text(line).toLowerCase().startsWith("history".toLowerCase())) { //play history info
            var pairs = text(line).split(":")(1).trim.split(",")
            for (pair <- pairs) {
              var player = Game.players.find(_.name == pair.split(";")(0)).get
              var cards = if (pair.split(";").size > 1) pair.split(";")(1).split("-").map(makeCard).toBuffer else Buffer[Card]()
              var temp = (player, cards)
              Game.history += temp
            }
          } else if (text(line).toLowerCase().startsWith("turn".toLowerCase())) { //turn info
            Game.turn = text(line).split(":")(1).trim.toInt
          } else if (text(line).toLowerCase().startsWith("dealer".toLowerCase())) { //dealer info
            Game.dealer = Some(Game.players(text(line).split(":")(1).trim.toInt))
          } else if (text(line).toLowerCase().startsWith("playerCount".toLowerCase())) { //player count info
            Game.playerCount = text(line).split(":")(1).trim.toInt
          }
          line = math.min(line + 1, text.size)
          fileRead = text.size == line //true if all the info is read
        } while (isValidLine)
      }
    }
  }
}