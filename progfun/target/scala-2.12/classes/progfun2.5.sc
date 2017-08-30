object session {
  val num1 = new Rational(1, 2)
  val num2 = new Rational(2, 3)

  num1
  num2.add(num1)


}

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom
    )

  override def toString: String =
    numer + "/" + denom
}