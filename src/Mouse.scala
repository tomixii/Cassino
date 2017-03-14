import processing.core._

class Mouse(p: PApplet) {
  
  def hover(x: Float, y: Float, width: Int, height: Int, onTop: Boolean): Boolean = {
    var w = width
    if(!onTop) 
      w -= 55
    p.mouseX > x && p.mouseX < x + w && p.mouseY > y && p.mouseY < y + height 
  }
}