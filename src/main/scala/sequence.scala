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
  private var savedColor: Color = Color.BLACK
  private var savedStroke: Stroke = new BasicStroke(1.0f)
  val g: Graphics2D

  def draw(): Unit

  protected def saveState() {
    savedColor = g.getColor
    savedStroke = g.getStroke
  }

  private def restoreState() {
    g.setColor(savedColor)
    g.setStroke(savedStroke)
  }

  protected def withPreservedState(body: => Unit) {
    saveState
    body
    restoreState
  }
}

class ObjectInstance (val name: String, val left: Int, val top: Int, val g: Graphics2D) extends Drawable {
  import ObjectInstance._
  private val metrics = g.getFontMetrics(Canvas.font)
  private val stringHeight = metrics.getAscent
  private val stringWidth = metrics.stringWidth(name)
  val height = stringHeight + strHeightPad
  val width = stringWidth + strWidthPad
  private val messages = new ListBuffer[Message]()

  override def draw() {
    withPreservedState {
      val objectStroke = new BasicStroke(3.0f)
      g.setStroke(objectStroke)
      val box = new RoundRectangle2D.Double(left, top, width, height, 0, 0)
      g.setColor(Color.WHITE)
      g.fill(box)
      g.setColor(Color.BLACK)
      g.draw(box)
      g.drawString(name, left + strWidthPad / 2, top + stringHeight + strHeightPad / 2)
    }
  }

  def middle = left + width / 2
  def right = left + width

  def addMessage(msg: Message): Unit = messages += msg
}

object ObjectInstance {
  private val strWidthPad = 20
  private val strHeightPad = 20
}

class Message(val name: String, val y: Int, val from: ObjectInstance, val to: ObjectInstance, val g: Graphics2D) extends Drawable {

  override def draw() {
    withPreservedState {
      drawArrowHead
      g.setColor(Color.BLACK)
      g.setStroke(new BasicStroke(3.0f))
      g.draw(new Line2D.Double(from.middle, y, to.middle - 15, y))
    }
  }

  private def drawArrowHead() {
    val xs = Array[Int](to.middle, to.middle - 15, to.middle - 15)
    val ys = Array[Int](y, y - 10, y + 10)
    val arrow = new Polygon(xs, ys, xs.length)
    g.setStroke(new BasicStroke(1.0f))
    g.setColor(Color.BLACK)
    g.draw(arrow)
    g.fill(arrow)
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
    val inst2 = new ObjectInstance("Message", inst1.left + inst1.width + 30, 0, g)
    val msg1 = new Message("draw", inst1.top + inst1.height + 30, inst1, inst2, g)
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
