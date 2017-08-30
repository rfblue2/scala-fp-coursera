abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other

  override def toString: String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int): IntSet = {
    if      (x < elem) new NonEmpty(x, left, right incl elem)
    else if (x > elem) new NonEmpty(x, left incl elem, right)
    else               this
  }
  def contains(x: Int): Boolean = {
    if      (x < elem) left  contains x
    else if (x > elem) right contains x
    else               true
  }
  override def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
  override def toString: String = "{" + left + elem + right + "}"
}

val t1 = new NonEmpty(2, Empty, Empty)
val t2 = t1 incl 3
val t3 = new NonEmpty(5, Empty, Empty)
val t4 = (t3 incl 6) incl 7

val t5 = t4 union t2
