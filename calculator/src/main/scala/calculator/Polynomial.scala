package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(), 2) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val discriminant = computeDelta(a,b,c)()
    if(discriminant<0) Signal(Set[Double]())
    else if(discriminant==0) Signal(Set[Double](0))
    else Signal(List(
      (-b() + Math.sqrt(discriminant)) / (2 * a()),
      (-b() - Math.sqrt(discriminant)) / (2 * a())
    ).toSet)
  }
}
