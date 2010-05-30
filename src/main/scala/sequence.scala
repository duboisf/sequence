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

class Instance (val name: String, val left: Int, val top: Int, val g: Graphics2D) extends Drawable {
  import Instance._
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

object Instance {
  private val strWidthPad = 20
  private val strHeightPad = 20
}

object Direction extends Enumeration {
  type Direction = Value
  val East, West = Value
}

abstract class Message extends Drawable {
  val name: String
  val y: Int
  val from: Instance
  val to: Instance
  val g: Graphics2D

  import Direction._

  protected def drawText(s: String, x: Int, y: Int) {

  }

  protected def drawLine(x1: Int, y1: Int, x2: Int, y2: Int) {
    g.setColor(Theme.foregroundColor)
    g.setStroke(Theme.lineStroke)
    g.draw(new Line2D.Double(x1, y1, x2, y2))
  }

  protected def drawArrowHead(x: Int, y: Int, direction: Direction) {
    // Make arrow point right or left
    val arrowBase = direction match {
      case East => x - Theme.arrowLength
      case West => x + Theme.arrowLength
      case direction_ => throw new IllegalArgumentException("Unknow direction '" + direction_)
    }
    val xs = Array(x, arrowBase, arrowBase)
    val ys = Array(y, y - Theme.arrowHalfBaseWidth, y + Theme.arrowHalfBaseWidth)
    val arrow = new Polygon(xs, ys, xs.length)
    g.setStroke(new BasicStroke(1.0f))
    g.setColor(Color.BLACK)
    g.draw(arrow)
    g.fill(arrow)
  }
}

class ArrowMessage(
  val name: String,
  val y: Int,
  val from: Instance,
  val to: Instance,
  val g: Graphics2D
) extends Message {

  import Direction._

  override def draw() {
    val op = (x: Int, y: Int) => if (from.left < to.left) x - y else x + y
    val x2 = if (from.left < to.left) to.middle - Theme.arrowLength else to.middle + Theme.arrowLength
    drawLine(from.middle, y, op(to.middle, Theme.arrowLength), y)
    val direction = if (from.left < to.left) East else West
    drawArrowHead(to.middle, y, direction)
  }
}

object Theme {
  val arrowLength = 15
  val arrowHalfBaseWidth = 10
  val lineStroke = new BasicStroke(3.0f)
  val backgroundColor = Color.WHITE
  val foregroundColor = Color.BLACK
  val font = new Font("Arial", Font.TRUETYPE_FONT, 20)
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

    val inst1 = new Instance("Sequence", 0, 0, g)
    val inst2 = new Instance("Message", inst1.left + inst1.width + 30, 0, g)
    val msg1 = new ArrowMessage("draw", inst1.top + inst1.height + 30, inst2, inst1, g)
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
