import scala.collection.mutable.ArrayBuffer
import java.lang.Math

object Donut {

  def main(arg: Array[String]): Unit = {
    var angle1 = 0.0
    var angle2 = 0.0
    val canvasSize = 1760
    val ascii: Array[Char] = ".,-~:;=!*#$@".toArray

    val zBuffer = ArrayBuffer.empty[Double]
    zBuffer.appendAll(Array.fill(canvasSize)(0.0))

    val pixels = ArrayBuffer.empty[Char]
    pixels.appendAll(Array.fill(canvasSize)(' '))

    print("\033[2J")

    var running: Boolean = true
    Runtime.getRuntime().addShutdownHook(
      new Thread {
        override def run(): Unit = {
          running = false
        }
      }
    )

    while (running) {
      //Clear canvas
      for (i <- 0 until canvasSize) {
        pixels.update(i, ' ')
        zBuffer.update(i, 0.0)
      }

      //Calculate the position and brightness of each pixel
      for (j <- 0 until 628 by 7) {
        for (i <- 0 until 628 by 2) {
          val sinI = Math.sin(i / 100.0)
          val cosJ = Math.cos(j / 100.0)
          val sinAngle1 = Math.sin(angle1)
          val sinJ = Math.sin(j / 100.0)
          val cosAngle1 = Math.cos(angle1)

          val height = cosJ + 2
          val distance = 1 / (sinI * height * sinAngle1 + sinJ * cosAngle1 + 5)
          val cosI = Math.cos(i / 100.0)

          val cosAngle2 = Math.cos(angle2)
          val sinAngle2 = Math.sin(angle2)
          val sinHeight = sinI * height * cosAngle1 - sinJ * sinAngle1

          val x = (40 + 30 * distance * (cosI * height * cosAngle2 - sinHeight * sinAngle2)).toInt
          val y = (12 + 15 * distance * (cosI * height * sinAngle2 + sinHeight * cosAngle2)).toInt

          val index = x + 80 * y
          val brightness = (ascii.size - 1).min((8 * ((sinJ * sinAngle1 - sinI * cosJ * cosAngle1) * cosAngle2 - sinI * cosJ * sinAngle1 - sinJ * cosAngle1 - cosI * cosJ * sinAngle2)).toInt)

          if (0 <= y && y < 22 && 0 <= x && x < 80 && distance > zBuffer(index)) {
            zBuffer.update(index, distance)
            pixels.update(index, ascii(if (brightness > 0) brightness else 0))
          }
        }
      }

      print("\033[H")
      print(pixels.toArray.mkString)
      println()

      angle1 = angle1 + 0.007
      angle2 = angle2 + 0.002
    }
  }
}
