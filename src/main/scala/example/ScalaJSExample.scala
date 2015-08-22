package example
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random

case class Point(x: Double, y: Double){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = Point(x - p.x, y - p.y)
  def /(d: Double) = Point(x / d, y / d)
  def *(d: Double) = Point(x * d, y * d)
  import math._
  def rotate(d: Double) = Point(
    x * cos(d) - y * sin(d),
    x * sin(d) + y * cos(d)
  )
}

@JSExport
object ScalaJSExample {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d")
                    .asInstanceOf[dom.CanvasRenderingContext2D]

    ctx.fillStyle = "white"
    ctx.fillRect(0, 0, 255, 255)
    ctx.fillStyle = "red"


    val fullAngle = 5.5 * math.Pi
    val interval = math.Pi / 8.2
    val segmentWidth = math.Pi / 10
    val size = 8.0
    val offsetGrowth = 1.042
    val thicknessRatio = 1.8
    ctx.translate(128, 128)

    for (i <- 0 until (fullAngle / interval).toInt) {
      ctx.beginPath()
      val p1 = Point(size * math.pow(offsetGrowth, i), 0).rotate(interval * i)
      ctx.moveTo(p1.x, p1.y)
      val p2 = p1 * thicknessRatio
      ctx.lineTo(p2.x, p2.y)
      val p3 = p2.rotate(segmentWidth)
      ctx.lineTo(p3.x, p3.y)
      val p4 = p1.rotate(segmentWidth)
      ctx.lineTo(p4.x, p4.y)
      ctx.closePath()
      ctx.fill()
    }
  }
}
