import scala.collection.mutable.Buffer

object Board {
  val x = 420
  val y = 190
  val width = 300
  val height = 300
  var cards = Buffer[Card]()
  var res = Buffer[Buffer[Card]]()

  def addCard(card: Card) = {
    cards += card
    cards = cards.sortBy(_.value)
  }

  def removeCard(cardBuff: Buffer[Card]) = {
    cards = cards -- cardBuff
  }

  def findPossibilities(card: Card, buff: Buffer[Buffer[Card]]): Buffer[(Card, Buffer[Buffer[Card]])] = {
    var apubuffer = Buffer[Buffer[Card]]()
    var result = Buffer[(Card, Buffer[Buffer[Card]])]()
    for (alku <- buff) {
      apubuffer.clear()
      apubuffer += alku
      for (buffer <- buff) {
        //        println("apu: " + apubuffer + " buffer: " + buffer + " -> " + (apubuffer.flatten.toSet & buffer.toSet).isEmpty)
        if ((apubuffer.flatten.toSet & buffer.toSet).isEmpty)
          apubuffer += buffer
        if (!result.exists(_._2.flatten.sortBy(_.value) == apubuffer.flatten.sortBy(_.value))) {
          var pair = (card, apubuffer.clone())
          result += pair
        }
      }
      //      res += apubuffer.clone()
    }
    //    println("alotus: " + buff)
    result
  }

  def takeCards(board: Buffer[Card], taken: Buffer[Card], sum: Int, initialSum: Int): Buffer[Buffer[Card]] = {
    if (board.isEmpty || sum == 0) {
      if (!res.exists(_.map(_.value) == taken.sortBy(_.value)) && taken.map(_.value).sum == initialSum)
        res += taken
    } else {
      for (card <- board) {
        takeCards(board.filter(_ != card), taken :+ card, sum - card.value, initialSum)
      }
    }
    res
  }
}