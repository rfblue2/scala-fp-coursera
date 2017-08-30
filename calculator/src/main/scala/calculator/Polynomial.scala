package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bVal = b.apply()
      bVal * bVal - 4 * a.apply() * c.apply()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val d = delta.apply()
      if (d < 0) Set()
      else if (d == 0) Set((-1 * b.apply() + math.sqrt(delta.apply())) / (2 * a.apply()))
      else Set((-1 * b.apply() + math.sqrt(delta.apply())) / (2 * a.apply()),
        (-1 * b.apply() - math.sqrt(delta.apply())) / (2 * a.apply()))
    }
  }
}
