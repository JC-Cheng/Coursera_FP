abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
  def == (that: Nat): Boolean
  def != (that: Nat) = !this.==(that)
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new Error("negative number")
  def == (that: Nat) = that.isZero
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def + (that: Nat) = new Succ(n + that)
  def - (that: Nat) = if (that.isZero) this else n - that.predecessor
  def == (that: Nat) = this.-(that).isZero
}

val _zero = Zero
val _one = Zero.successor
val _two = Zero.successor.successor

_two.predecessor.==(_one)
_two.==(_one.+(_one))
_two.predecessor

Zero.successor.!=(_two.predecessor)
