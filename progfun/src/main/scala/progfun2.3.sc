import math.abs
object session {
  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / 2) < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(next, guess)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double): Double =
    fixedPoint(averageDamp(y => x/y))(1.0)

  sqrt(2)

}