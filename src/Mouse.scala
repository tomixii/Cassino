import processing.core._

class Mouse(p: PApplet) {
  
  var up = true
  
  def hover(x: Float, y: Float, width: Int, height: Int): Boolean = {
    var w = width
    p.mouseX > x && p.mouseX < x + w && p.mouseY > y && p.mouseY < y + height 
  }
  
  def clicked: Boolean = {
    if(up && p.mousePressed){
      println("clicked")
      up = !p.mousePressed
      true
    }else
    	up = !p.mousePressed
      false
    	
//    if(p.mousePressed)
//      if(!p.mousePressed)
//        true
//      else 
//        false
//    else
//      false
  }
}