abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat

  def + (that: Nat): Nat = {
    if (that.isZero) this
    else this.successor + that.predecessor
  }

  def - (that: Nat): Nat = {
    if (that.isZero) this
    else this.predecessor - that.predecessor
  }
}

object Zero extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = throw new Error("Zero does not have predecessor")

  def successor: Nat = new Succ(this)
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false

  def predecessor: Nat = n

  def successor: Nat = new Succ(this)

}

