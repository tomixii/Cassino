import processing.core._

class Mouse(p: PApplet) {
  
  def hover(x: Float, y: Float, width: Int, height: Int): Boolean = {
    var w = width
    p.mouseX > x && p.mouseX < x + w && p.mouseY > y && p.mouseY < y + height 
  }
  
  def clicked: Boolean = {
    if(p.mousePressed)
      if(!p.mousePressed)
        true
      else 
        false
    else
      false
  }
}