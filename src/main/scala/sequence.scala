package sequence

import java.util._
import java.awt.image.BufferedImage
import java.awt._
import java.awt.font._
import java.awt.geom._
import java.io.File
import javax.imageio.ImageIO

trait Drawable {
  def draw(g: Graphics2D): Unit
}

class ObjectInstance (val name: String) extends Drawable {
  var x: Int = 40
  var y: Int = 40

  override def draw(g: Graphics2D) {
    val metrics = g.getFontMetrics(Canvas.font)
    val strHeight = metrics.getHeight
    val strWidth = metrics.stringWidth(name)
    g.fill(new RoundRectangle2D.Double(x, y, strWidth + 20, strHeight + 4, 30, 30))
    g.drawString(name, x + 10, y + strHeight - 8)
  }
}

class Message(val name: String, val from: ObjectInstance, val to: ObjectInstance) {
}

object Canvas {
  val gradient = new GradientPaint(0, 0, Color.BLUE, 200, 100, Color.WHITE);
  val font = new Font("Arial", Font.TRUETYPE_FONT, 40)

  def draw() {
    val buffIm = new BufferedImage(400, 400, BufferedImage.TYPE_INT_ARGB)
    val g = buffIm.createGraphics()

    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setPaint(gradient)

    val map = new Hashtable[TextAttribute, Object]()
    map.put(TextAttribute.FOREGROUND, Color.BLACK);
    
    g.setFont(font.deriveFont(map))

    val inst = new ObjectInstance("Sequence")
    inst.draw(g)
    ImageIO.write(buffIm, "PNG", new File("test2.png"))
  }
}

object Sequence {
  def main(args: Array[String]) {
    val inst = new ObjectInstance("fred")
    val buffIm = new BufferedImage(400, 400, BufferedImage.TYPE_INT_ARGB)
    val g = buffIm.createGraphics()
    g.drawLine(20, 100, 120, 100)

    val gradient = new GradientPaint(0, 0, Color.BLUE, 200, 100, Color.WHITE);

    g.setPaint(gradient)
    g.fill(new RoundRectangle2D.Double(10, 10, 200, 100, 30, 30))

    val arial = new Font("Arial", Font.TRUETYPE_FONT, 40)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    val map = new Hashtable[TextAttribute, Object]()
    map.put(TextAttribute.FOREGROUND, Color.BLACK);
    val blueArial = arial.deriveFont(map)
    g.setFont(blueArial)
    g.drawString("Hello, World!", 40, 300)
//    GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames().map(println)
//    val arial = Font.createFont(Font.TRUETYPE_FONT, new File("Arial.ttf"))
    ImageIO.write(buffIm, "PNG", new File("test.png"))
    Canvas.draw()
  }
}

// vim: set ts=2 sw=2 et:
