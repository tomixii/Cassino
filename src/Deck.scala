import scala.collection.mutable.Buffer
import scala.util.Random
import processing.core._
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

object Deck {
  
	var h01 = new PImage
  var h02 = new PImage
  var h03 = new PImage
  var h04 = new PImage
  var h05 = new PImage
  var h06 = new PImage
  var h07 = new PImage
  var h08 = new PImage
  var h09 = new PImage
  var h10 = new PImage
  var h11 = new PImage
  var h12 = new PImage
  var h13 = new PImage
  
  var s01 = new PImage
  var s02 = new PImage
  var s03 = new PImage
  var s04 = new PImage
  var s05 = new PImage
  var s06 = new PImage
  var s07 = new PImage
  var s08 = new PImage
  var s09 = new PImage
  var s10 = new PImage
  var s11 = new PImage
  var s12 = new PImage
  var s13 = new PImage
  
  var d01 = new PImage
  var d02 = new PImage
  var d03 = new PImage
  var d04 = new PImage
  var d05 = new PImage
  var d06 = new PImage
  var d07 = new PImage
  var d08 = new PImage
  var d09 = new PImage
  var d10 = new PImage
  var d11 = new PImage
  var d12 = new PImage
  var d13 = new PImage
  
  var c01 = new PImage
  var c02 = new PImage
  var c03 = new PImage
  var c04 = new PImage
  var c05 = new PImage
  var c06 = new PImage
  var c07 = new PImage
  var c08 = new PImage
  var c09 = new PImage
  var c10 = new PImage
  var c11 = new PImage
  var c12 = new PImage
  var c13 = new PImage
  
  var cardback = new PImage
  
  var deck = Stack[Card]()
  var cardImages = Map[String, PImage]()
  var cardStrings = Buffer[String]("h01", "s01", "d01", "c01",
                                   "h02", "s02", "d02", "c02",
                                   "h03", "s03", "d03", "c03",
                                   "h04", "s04", "d04", "c04",
                                   "h05", "s05", "d05", "c05",
                                   "h06", "s06", "d06", "c06",
                                   "h07", "s07", "d07", "c07",
                                   "h08", "s08", "d08", "c08",
                                   "h09", "s09", "d09", "c09",
                                   "h10", "s10", "d10", "c10",
                                   "h11", "s11", "d11", "c11",
                                   "h12", "s12", "d12", "c12",
                                   "h13", "s13", "d13", "c13")
  
  def shuffleDeck = {
    for(i <- 0 until 52){
    	deck.push(new Card(i / 4 + 1, i % 4, i, this.cardImages(cardStrings(i))))
    }
    deck = Random.shuffle(deck)
  }
}