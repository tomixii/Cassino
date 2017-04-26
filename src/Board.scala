import scala.collection.mutable.Buffer

object Board {
  val x = 420 //board coordinates
  val y = 190
  val width = 300 //board dimensions
  val height = 300
  var cards = Buffer[Card]() //cards on board
  var res = Buffer[Buffer[Card]]() //helping buffer

  def addCard(card: Card) = {
    cards += card
    cards = cards.sortBy(_.value) // board is sorted by value 
  }

  def removeCards(cardBuff: Buffer[Card]) = {
    cards = cards -- cardBuff
  }
  
  // This method finds all the combinations which can be collected from board with certain card 
  def takeCards(board: Buffer[Card], taken: Buffer[Card], sum: Int, initialSum: Int): Buffer[Buffer[Card]] = {
	  if (board.isEmpty || sum == 0) { //if there's no cards anymore to check or sum has been reached
		  if (!res.exists(_.map(_.value) == taken.sortBy(_.value)) && taken.map(_.value).sum == initialSum) // add a possibility if that combination isn't at combinations yet 
			  res += taken
	  } else {
		  for (card <- board) {
			  takeCards(board.filter(_ != card), taken :+ card, sum - card.value, initialSum) //recursively find all the possibilities
		  }
	  }
	  res
  }

  def findPossibilities(card: Card, buff: Buffer[Buffer[Card]]): Buffer[(Card, Buffer[Buffer[Card]])] = { //finds all possibilities so that same card isn't in the list twice
    var temp = Buffer[Buffer[Card]]() //temporary buffer where cardcombinations are stored temporarily
    var result = Buffer[(Card, Buffer[Buffer[Card]])]()
    for (first <- buff) {
      temp.clear()
      temp += first //adds first combination to buffer
      for (buffer <- buff) {
        if (!result.exists(_._2.flatten.sortBy(_.value) == temp.flatten.sortBy(_.value))) { //checks if final result contains already buffer of these combinations 
          var pair = (card, temp.clone())
          result += pair //if not, add new combination
        }
        if ((temp.flatten.toSet & buffer.toSet).isEmpty) // add new combination to combinationbuffer if there isn't equivalent in the buffer
          temp += buffer
      }
    }
    result // returns all possible combinations where same card isn't used twice
  }

}