
// covariant class
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def toString: String
  def prepend [U >: T] (elem: U): List[U] = Cons(elem, this)
}

// case class
case class Cons[T](head: T, tail: List[T]) extends List[T] {
  def isEmpty: Boolean= false
  override def toString: String = head + ", " + tail
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString: String = "Nil"
}
//


//
abstract class str_int {
  def s: String
  def i: Int
  def toString: String
}
class only_str(val s: String) extends str_int {
  val i = 0
  override def toString: String = "(" + s + ", " + i + ")"
}
class only_int(val i: Int) extends str_int {
  val s = "-"
  override def toString: String = "[" + s + ", " + i + "]"
}
//

val s = new only_str("A")
val v: List[String] = Nil

def f(xs: List[only_str], x: only_int) = xs.prepend(x)

val L = Cons(new only_str("Hey"), Cons(new only_str("You"), Nil))
val e = new only_int(777)

// List[str_int]
f(L, e)


//
def insert_sort(xs: List[Int]): List[Int] = {
  if (xs.isEmpty) Nil
  else insert(xs.head, insert_sort(xs.tail))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => Cons(x, Nil)
  case Cons(y, ys) => if (x < y) xs.prepend(x) else insert(x, ys).prepend(y)
}

val A = Nil.prepend(5).prepend(3).prepend(6).prepend(0)
insert_sort(A)