trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T
  override def toString: String = head + ", " + tail
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
  def nth(n: Int): T = {
    if (n == 0) head
    else if (n > 0) tail.nth(n - 1)
    else throw new IndexOutOfBoundsException("Out of bounds")
  }
}

class Nil[T]() extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  def nth(n: Int): Nothing = throw new IndexOutOfBoundsException("Out of Bounds")
  override def toString: String = ""
}

object List {
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply() = new Nil
}