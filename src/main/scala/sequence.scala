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

class Instance(val name: String, val left: Int, val top: Int, val g: Graphics2D) extends Drawable {
  import Canvas.Theme
  private val stringHeight = Theme.instanceMetrics.getAscent
  private val stringWidth = Theme.instanceMetrics.stringWidth(name)
  val height = stringHeight + Theme.stringPadding * 2
  val width = stringWidth + Theme.stringPadding * 2
  val x = left + width / 2
  val y = top + height / 2
  val right = left + width
  val bottom = top + height

  override def draw() {
    withPreservedState {
      g.setStroke(Theme.lineStroke)
      val box = new RoundRectangle2D.Double(left, top, width, height, 0, 0)
      g.setColor(Theme.backgroundColor)
      g.fill(box)
      g.setColor(Theme.foregroundColor)
      g.draw(box)
      g.drawString(name, left + Theme.stringPadding, top + stringHeight + Theme.stringPadding)
    }
  }
}

object Direction extends Enumeration {
  type Direction = Value
  val East, West = Value
}

abstract class Message extends Drawable {
  import Direction._
  import Canvas.Theme

  val name: String
  val top: Int
  val from: Instance
  val to: Instance
  val g: Graphics2D
  val y: Int

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
  val top: Int,
  val from: Instance,
  val to: Instance,
  val g: Graphics2D
) extends Message {
  import Direction._
  import Canvas.Theme

  val y = top + (Theme.messageMetrics.getAscent + Theme.stringPadding) / 2

  override def draw() {
    val op = (x: Int, y: Int) => if (from.x < to.x) x - y else x + y
    val x2 = if (from.x < to.x) to.x - Theme.arrowLength else to.x + Theme.arrowLength
    drawLine(from.x, y, op(to.x, Theme.arrowLength), y)
    val direction = if (from.x < to.x) East else West
    drawArrowHead(to.x, y, direction)
  }
}

object Canvas {
  private val bufferedImage = new BufferedImage(400, 400, BufferedImage.TYPE_INT_ARGB)
  val g = bufferedImage.createGraphics()

  object Theme {
    val arrowLength = 15
    val arrowHalfBaseWidth = 10
    val lineStroke = new BasicStroke(3.0f)
    val backgroundColor = Color.WHITE
    val foregroundColor = Color.BLACK
    val instanceFont = new Font("Arial", Font.TRUETYPE_FONT, 20)
    val messageFont = new Font("Arial", Font.TRUETYPE_FONT, 20)
    val stringPadding = 10
    val instanceMetrics = g.getFontMetrics(instanceFont)
    val messageMetrics = g.getFontMetrics(messageFont)
  }

  def draw() {

    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    val map = new Hashtable[TextAttribute, Object]()
    map.put(TextAttribute.FOREGROUND, Color.BLACK);
    
    g.setFont(Theme.instanceFont.deriveFont(map))

    val inst1 = new Instance("Sequence", 10, 10, g)
    val inst2 = new Instance("Message", inst1.right + 30, 10, g)
    val msg1 = new ArrowMessage("draw", inst1.bottom + 30, inst2, inst1, g)
    val l = scala.List(inst1, inst2, msg1)
    l.map(_.draw)
    ImageIO.write(bufferedImage, "PNG", new File("test2.png"))
  }
}

object Sequence {
  def main(args: Array[String]) {
    Canvas.draw()
  }
}

// vim: set ts=2 sw=2 et:
