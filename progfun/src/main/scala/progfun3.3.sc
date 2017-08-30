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

val l1 = new Cons(1, new Nil[Int])
val l2 = new Cons(2, l1)
val l3 = new Cons(3, l2)
l3.nth(0)
l3.nth(1)
l3.nth(2)
l3.nth(3)


