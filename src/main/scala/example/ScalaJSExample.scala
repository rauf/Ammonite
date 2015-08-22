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
//    canvas.style.width = "16px"
//    canvas.style.height = "16px"


    val fullAngle = 4 * math.Pi
    val interval = math.Pi / 4.0

    val startSize = 20
    val startAngle = math.Pi * 0.05

    val segmentGap = 5
    val radialGap = 5

    val thicknessRatio = 1.8
    val shearRatio = 0.2

    val intervalGrowth = math.pow(thicknessRatio, interval / math.Pi / 2 )
    ctx.translate(128, 128)

    def points(i: Int) = {
      val radius = startSize * math.pow(intervalGrowth, i)
      val angle = interval * -i + startAngle
      val p1 = Point(radius, 0).rotate(angle)
      val radialGapV = Point(radialGap, 0).rotate(angle)
      val shear = Point(0, -radius * shearRatio).rotate(angle)
      val p2 = p1 * thicknessRatio - radialGapV + shear

      val scaledSegmentGap = math.min(segmentGap / radius, interval / 3)
      val p3 = p2.rotate(interval - scaledSegmentGap) / intervalGrowth
      val p4 = p1.rotate(interval - scaledSegmentGap) / intervalGrowth
      (p1, p2, p3, p4)
    }
    def draw(color: String, points: Point*) = {
      ctx.beginPath()
      ctx.fillStyle = color
      ctx.moveTo(points(0).x, points(0).y)
      for(p <- points.tail){
        ctx.lineTo(p.x, p.y)
      }
      ctx.closePath()
      ctx.fill()
    }
    val turns = (fullAngle / interval).toInt

    for (i <- 0 until turns) {
      val (p1, p2, p3, p4) = points(i)
      draw("rgb(220, 0, 0)", p1, p2, p3, p4)
    }

    ctx.beginPath()
    ctx.arc(0, 0, startSize * 0.75 - radialGap, 0, 2 * math.Pi)
    ctx.closePath()
    ctx.fill()
  }
}
