package sequence

import java.util.Hashtable
import java.awt.image.BufferedImage
import java.awt._
import java.awt.font._
import java.awt.geom._
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.ListBuffer

trait Drawable {
  def draw(): Unit
}

class ObjectInstance (val name: String, val x: Int, val y: Int, val g: Graphics2D) extends Drawable {
  import ObjectInstance._
  private val metrics = g.getFontMetrics(Canvas.font)
  val height = metrics.getAscent
  val width = metrics.stringWidth(name)
  private val messages = new ListBuffer[Message]()

  override def draw() {
    val oldColor = g.getColor
    val stroke = g.getStroke
    val objectStroke = new BasicStroke(3.0f)
    g.setStroke(objectStroke)
    val box = new RoundRectangle2D.Double(x, y, width + strWidthPad, height + strHeightPad, 0, 0)
    g.setColor(Color.WHITE)
    g.fill(box)
    g.setColor(Color.BLACK)
    g.draw(box)
    g.setColor(oldColor)
    g.drawString(name, x + strWidthPad / 2, y + height + strHeightPad / 2)
    g.setStroke(stroke)
  }

  def addMessage(msg: Message): Unit = messages += msg
}

object ObjectInstance {
  private val strWidthPad = 20
  private val strHeightPad = 20
}

class Message(val name: String, val x: Int, val y: Int, val to: ObjectInstance, val g: Graphics2D) extends Drawable {

  override def draw() {
    g.draw(new Line2D.Double(x, y, to.x + to.width / 2, y))
  }
}

object Canvas {
  val gradient = new GradientPaint(0, 0, Color.BLUE, 200, 100, Color.WHITE);
  val font = new Font("Arial", Font.TRUETYPE_FONT, 20)
}

class Canvas {

  import Canvas._

  def draw() {
    val buffIm = new BufferedImage(400, 400, BufferedImage.TYPE_INT_ARGB)
    val g = buffIm.createGraphics()

    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    val map = new Hashtable[TextAttribute, Object]()
    map.put(TextAttribute.FOREGROUND, Color.BLACK);
    
    g.setFont(font.deriveFont(map))

    val inst1 = new ObjectInstance("Sequence", 0, 0, g)
    val inst2 = new ObjectInstance("Message", inst1.x + inst1.width + 30, 0, g)
    val msg1 = new Message("draw", inst1.x + inst1.width / 2, inst1.y + inst1.height + 30, inst2, g)
    val l = scala.List(inst1, inst2, msg1)
    l.map(_.draw)
    ImageIO.write(buffIm, "PNG", new File("test2.png"))
  }
}

object Sequence {
  def main(args: Array[String]) {
    val canvas = new Canvas
    canvas.draw()
  }
}

// vim: set ts=2 sw=2 et:
