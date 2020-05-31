package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4 * a() * c() )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val x1 = (- b() + delta() )/ (2*a())
    val x2 = (- b() - delta() )/ (2*a())
    Signal(Set(x1,x2))
  }
}
