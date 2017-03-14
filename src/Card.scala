import processing.core._

// suits: 0 = hearts, 1 = spades, 2 = diamonds, 3 = clubs

class Card(val value: Int, val suit: Int, val number: Int, val img: PImage) {
  var active: Boolean = false
  var x = 0f
  var y = 0f
  var width = 70
  var height = 105
  
  def valueOnHand: Int = {
    if(value == 1)
      14
    else if(value == 2 && suit == 1)
      15
    else if(value == 10 && suit == 2)
      16
    else
      value
  }
}