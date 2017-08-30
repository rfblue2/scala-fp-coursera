package idealized
// lesson 4.1
abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T): Boolean

  def && (x: => Boolean): Boolean = ifThenElse(x, fals)
  def || (x: => Boolean): Boolean = ifThenElse(tru, x)
  def unary_! = ifThenElse(fals, tru)

  def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)
  def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)
  def < (x: Boolean): Boolean = ifThenElse(fals, x)

}

object tru extends Boolean {
  def ifThenElse[T](t: => T, e: => T): Boolean = t
}

object fals extends Boolean {
  def ifThenElse[T](t: => T, e: => T): Boolean = e
}