package sequence

import java.util.Hashtable
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, Font, BasicStroke, Polygon, Color, RenderingHints}
import java.awt.font._
import java.awt.geom._
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.{Map, ListBuffer, HashMap}
import scala.io.Source
import scala.util.parsing.combinator._

trait Drawable {
  val g: Graphics2D

  def draw(): Unit

  protected def withPreservedState(action: => Unit) {
    val color = g.getColor
    val stroke = g.getStroke
    action
    g.setColor(color)
    g.setStroke(stroke)
  }
}

trait Named {
  import SequenceDiagram.Theme

  val name: String

  val stringWidth = Theme.instanceMetrics.stringWidth(name)
  val stringHeight = Theme.instanceMetrics.getAscent
}

class Instance(val name: String, var left: Int, var top: Int, val g: Graphics2D) extends Drawable with Named {
  import SequenceDiagram.Theme

  def height = stringHeight + Theme.instanceStringPadding * 2
  def width = stringWidth + Theme.instanceStringPadding * 2
  def x = left + width / 2
  def y = top + height / 2
  def right = left + width
  def bottom = top + height

  override def draw() {
    g.setStroke(Theme.lineStroke)
    val box = new RoundRectangle2D.Double(left, top, width, height, 0, 0)
    g.setColor(Theme.backgroundColor)
    g.fill(box)
    g.setColor(Theme.foregroundColor)
    g.draw(box)
    g.drawString(name, left + Theme.instanceStringPadding, top + stringHeight + Theme.instanceStringPadding)
  }

  override def toString = name + "(left: " + left + ", top: " + top + ", width: " + width + ", height: " + height + ")"
}

object Direction extends Enumeration {
  type Direction = Value
  val East, West = Value
}

abstract class Message extends Drawable {
  import Direction._
  import SequenceDiagram.Theme

  val name: String
  val top: Int
  val from: Instance
  val to: Instance
  val g: Graphics2D
  val height: Int

  def width: Int

  def bottom = top + height
  def x = left + Math.abs(from.x - to.x) / 2
  def left = if (from.x < to.x) from.x else to.x
  def y = top + height / 2

  protected def drawText(s: String, x: Int, y: Int) {
    g.setColor(Theme.foregroundColor)
    g.drawString(s, x, y)
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
      case huh => throw new IllegalArgumentException("Unknow direction '" + huh)
    }
    val xs = Array(x, arrowBase, arrowBase)
    val ys = Array(y, y - Theme.arrowHalfBaseWidth, y + Theme.arrowHalfBaseWidth)
    val arrow = new Polygon(xs, ys, xs.length)
    g.setStroke(new BasicStroke(1.0f))
    g.setColor(Color.BLACK)
    g.draw(arrow)
    g.fill(arrow)
  }

  override def toString = from.name + " -> " + to.name + ": " + name + " (left: " + left + " top: " + top + ", width: " + width + ", height: " + height + ")"
}

class ArrowMessage(
  val name: String,
  val top: Int,
  val from: Instance,
  val to: Instance,
  val g: Graphics2D
) extends Message with Named {
  import Direction._
  import SequenceDiagram.Theme

  def width = Math.abs(to.x - from.x)
  val height = Theme.messageMetrics.getAscent + Theme.messageStringPadding

  override def draw() {
    if (stringWidth > width) {
      to.left += stringWidth - width + 40
    }
    val op = (x: Int, y: Int) => if (from.x < to.x) x - y else x + y
    val x2 = if (from.x < to.x) to.x - Theme.arrowLength else to.x + Theme.arrowLength
    val nameX = x - Theme.messageMetrics.stringWidth(name) / 2
    val nameY = top + Theme.messageMetrics.getAscent
    val lineY = nameY + Theme.messageStringPadding
    drawText(name, nameX, nameY)
    drawLine(from.x, lineY, op(to.x, Theme.arrowLength), lineY)
    val direction = if (from.x < to.x) East else West
    drawArrowHead(to.x, lineY, direction)
  }
}

object SequenceDiagram {
  private val instances: Map[String, Instance] = new HashMap[String, Instance]()
  private val messages = new ListBuffer[Message]()
  private var instanceX = 10
  private var messageY = 60
  private val bufferedImage = new BufferedImage(400, 400, BufferedImage.TYPE_INT_ARGB)
  val g = bufferedImage.createGraphics()

  object Theme {
    val arrowLength = 15
    val arrowHalfBaseWidth = 7
    val lineStroke = new BasicStroke(3.0f)
    val backgroundColor = Color.WHITE
    val foregroundColor = Color.BLACK
    val instanceFont = new Font("Arial", Font.TRUETYPE_FONT, 20)
    val messageFont = new Font("Arial", Font.TRUETYPE_FONT, 20)
    val instanceStringPadding = 10
    val messageStringPadding = 5
    val instanceMetrics = g.getFontMetrics(instanceFont)
    val messageMetrics = g.getFontMetrics(messageFont)
  }

  def draw() {
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    val map = new Hashtable[TextAttribute, Object]()
    map.put(TextAttribute.FOREGROUND, Color.BLACK);
    
    g.setFont(Theme.instanceFont.deriveFont(map))

    messages map {
      msg => {
        println(msg)
        msg.draw
      }
    }
    instances foreach (t => println(t._2))
    instances foreach (t => t._2.draw)
    ImageIO.write(bufferedImage, "PNG", new File("test2.png"))
  }

  def fromFile(filename: String) {
    val source = Source.fromFile(new File(filename))
    val parser = new Parser
    source.getLines.foreach(line => dispatchResult(parser.run(line)))
    messages map println
  }

  private def dispatchResult(parserResult: ParsedExpression) {
    parserResult match {
      case ParsedArrowMessage(src, msg, dest) => {
        val srcInstance = getInstance(src)
        val destInstance = getInstance(dest)
        messages += new ArrowMessage(msg, messageY, srcInstance, destInstance, g)
        messageY = messages.last.bottom + 20
      }
    }
  }

  private def getInstance(name: String): Instance = {
    if (!instances.contains(name))
    {
      val instance = new Instance(name, instanceX, 10, g)
      instanceX += instance.right + 10
      instances(name) = instance
      instance
    } else instances(name)
  }
}

abstract sealed class ParsedExpression

case class ParsedArrowMessage(val src: String, val msg: String, val dest: String) extends ParsedExpression

class Parser extends RegexParsers {

  def instance: Parser[String] = regex("\\w+".r)
  def message: Parser[String] = regex("(.+\\s*)+".r)
  def expression: Parser[ParsedExpression] = (instance ~ "->" ~ instance ~ ':' ~ message) ^^ {
    case src ~ arrow ~ dest ~ colon ~ msg => ParsedArrowMessage(src, msg, dest)
  }

  def run(input: String): ParsedExpression = parseAll(expression, input) match {
    case Success(result, _) => result
    case Failure(msg, _) => throw new Exception("Parsing failure: " + msg)
    case Error(msg, _) => throw new Exception("Parsing error: " + msg)
  }
}

object Sequence extends Parser {
  def main(args: Array[String]) {
    SequenceDiagram.fromFile("testInput.txt")
    SequenceDiagram.draw
    val tokens = parseAll(expression, "something -> other: test this!") match {
      case Success(result, _) => result
      case _ => throw new Exception("Error parsing expression")
    }
    println(tokens)
  }
}

// vim: set ts=2 sw=2 et:
