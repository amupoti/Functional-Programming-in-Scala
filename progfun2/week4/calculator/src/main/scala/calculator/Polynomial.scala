package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    println("Delta is "+(Math.pow(b(), 2) - (4 * a() * c())))
    Signal(Math.pow(b(), 2) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(delta() match {
      case d if d < 0 => Set[Double]()
      case d if d == 0 => Set[Double](-1*b()/2*a())
      case d => Set[Double]((-1*b()+Math.sqrt(d))/(2*a()),(-1*b()-Math.sqrt(d))/(2*a()))
    })
  }
}
