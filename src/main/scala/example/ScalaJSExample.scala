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
  def length = sqrt(x*x + y*y)
  def rotate(d: Double) = Point(
    x * cos(d) - y * sin(d),
    x * sin(d) + y * cos(d)
  )
}

@JSExport
object ScalaJSExample {
  @JSExport
  def main(): Unit = {
    val canvas = scalatags.JsDom.all.canvas.render: html.Canvas
    dom.document.body.innerHTML = ""
    dom.document.body.appendChild(canvas)
    val imageHeight = 255
    canvas.style.display = "block"
    canvas.style.cssFloat = "left"
    canvas.width = imageHeight
    canvas.height = imageHeight
    val ctx = canvas.getContext("2d")
                    .asInstanceOf[dom.CanvasRenderingContext2D]

    ctx.fillStyle = "white"
    ctx.fillRect(0, 0, imageHeight, imageHeight)
//    canvas.style.width = "16px"
//    canvas.style.height = "16px"


    val fullAngle = 4 * math.Pi
    val interval = math.Pi / 4.5

    val startSize = 9.6
    val startAngle = math.Pi * 0.2

    val segmentGap = startSize/1.5
    val radialGap = startSize/6

    val thicknessRatio = 2.5
    val shearRatio = 0.2

    val intervalGrowth = math.pow(thicknessRatio, interval / math.Pi / 2 )
    ctx.translate(imageHeight/2, imageHeight/2)


    val darkRed = 160
    val brightRed = 255

    def greyColor(c: Int) = s"rgb(${c/4}, ${c/4}, ${c/4})"
    def redColor(c: Int) = s"rgb($c, 0, 0)"
    def pointsFor(i: Int) = {
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
    def getGradient(p1: Point, p2: Point) = {
      val d = (p2 - p1) / (p2 - p1).length
      val range = (brightRed - darkRed) / 2 * d.y
      val avg = (brightRed + darkRed) / 2
      val startColor = (avg + range).toInt
      val endColor = (avg - range).toInt
      (startColor, endColor)
    }
    def draw(color: Int => String, points: Point*) = {
      ctx.beginPath()
      val p1 = points(3)
      val p2 = points(2)
      val (c1, c2) = getGradient(p1, p2)
      val gradient = ctx.createLinearGradient(p1.x, p1.y, p2.x, p2.y)


      gradient.addColorStop(0, color(c1.toInt))
      gradient.addColorStop(1, color(c2.toInt))
      ctx.fillStyle = gradient
      ctx.moveTo(points(0).x, points(0).y)
      for(p <- points.tail) ctx.lineTo(p.x, p.y)
      ctx.closePath()
      ctx.fill()
    }
    val turns = (fullAngle / interval).toInt

    for (i <- 0 until turns - 1) {
      val (p1, p2, p3, p4) = pointsFor(i)
      val (p1x, p2x, p3x, p4x) = pointsFor(i+1)
      draw(greyColor, p1x, p2, p3, p4x)
    }
    for (i <- 0 until turns) {
      val (p1, p2, p3, p4) = pointsFor(i)
      draw(redColor, p1, p2, p3, p4)
    }

    val spotRadius = startSize * 0.9 - radialGap
    val gradient = ctx.createLinearGradient(0, 0 - spotRadius, 0, 0 + spotRadius)
    gradient.addColorStop(0, s"rgb($brightRed, 0, 0)")
    gradient.addColorStop(1, s"rgb($darkRed, 0, 0)")
    ctx.fillStyle = gradient
    ctx.beginPath()
    ctx.arc(0, 0, spotRadius, 0, 2 * math.Pi)
    ctx.closePath()
    ctx.fill()
    import scalatags.JsDom.svgAttrs.{fill, points, width, height, id, offset, stopColor, stopOpacity, x1, x2, y1, y2, xmlns, cx, cy, r}
    import scalatags.JsDom.implicits._
    import scalatags.JsDom.svgTags._
    import scalatags.JsDom.styles.float
    val c = imageHeight / 2
    def svgStripe(color: Int => String, p1: Point, p2: Point, p3: Point, p4: Point, i: Int) = {
      val (c1, c2) = getGradient(p4, p3)
      val ps = Seq(p1, p2, p3, p4)
      val minX = ps.map(_.x).min
      val minY = ps.map(_.y).min
      val maxX = ps.map(_.x).max
      val maxY = ps.map(_.y).max
      def toPct(d: Double, min: Double, max: Double) = (d - min) / (max - min)
      Seq(
        "linearGradient".tag(implicitly)(
          id := s"gradient$i",
          x1 := toPct((p4.x + p1.x)/2, minX, maxX),
          y1 := toPct((p4.y + p1.y)/2, minY, maxY),
          x2 := toPct((p3.x + p2.x)/2, minX, maxX),
          y2 := toPct((p3.y + p2.y)/2, minY, maxY),
          stop(offset := "0", stopColor := color(c1)),
          stop(offset := "1", stopColor := color(c2))
        ),
        polygon(
          fill := s"url(#gradient$i)",
          points := {
            Seq(p1, p2, p3, p4).map{p => (p.x + c) + " " + (p.y + c)}
              .mkString(", ")
          }
        )
      )
    }
    val svgContainer = svg(
      xmlns:="http://www.w3.org/2000/svg",
      width := imageHeight,
      height := imageHeight,
      float := "left",
      for (i <- 0 until turns - 1) yield {
        val (p1, p2, p3, p4) = pointsFor(i)
        val (p1x, p2x, p3x, p4x) = pointsFor(i+1)
        svgStripe(greyColor, p1x, p2, p3, p4x, i + 9999)
      },
      for (i <- 0 until turns) yield {
        val (p1, p2, p3, p4) = pointsFor(i)
        svgStripe(redColor, p1, p2, p3, p4, i)
      },
      "linearGradient".tag(implicitly)(
        id := s"gradientCircle",
        x1 := 0,
        y1 := 1,
        x2 := 0,
        y2 := 0,
        stop(offset := "0", stopColor := s"rgb($darkRed, 0, 0)"),
        stop(offset := "1", stopColor := s"rgb($brightRed, 0, 0)")
      ),
      circle(
        cx := c, cy := c, r := spotRadius,
        fill := "url(#gradientCircle)"
      )
    )

    dom.document.body.appendChild(svgContainer.render)
  }
}
