import processing.core._

object Scoreboard {
  val x = 100  //scoreboard coordiantes
  val y = 100
  val width = 940 //scoreboard dimensions
  val height = 480
  var isPressed = false //TAB is pressed
  val pointStrings = Vector[String]("SCORE", "SPADES", "ACES", "COLLECTED", "SWEEPS") //column headers
}