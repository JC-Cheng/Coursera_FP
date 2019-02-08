object IntSet{
  abstract class IntSet {
    def incl(x: Int): IntSet
    def contain(x: Int): Boolean
    def union(other: IntSet): IntSet

    def toList: List[Int]
  }

  object Empty extends IntSet {
    def incl(x: Int) = NonEmpty(x, Empty, Empty)
    def contain(x: Int) = false
    def union(other: IntSet): IntSet = other

    def toList: List[Int] = List()
    override def toString = "{}"
  }

  case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def incl(x: Int): IntSet = {
      if (x < elem) NonEmpty(elem, left incl x, right)
      else if (x > elem) NonEmpty(elem, left, right incl x)
      else this
    }

    def contain(x: Int): Boolean = {
      if (x < elem) left contain x
      else if (x > elem) right contain x
      else true
    }

    def union(other: IntSet): IntSet = {
      (left union (right union other )) incl elem
    }

    def toList: List[Int] = {
      left.toList ::: elem :: right.toList
    }

    override def toString = {
      "{" + toList.mkString(", ") + "}"
    }
  }
}

val x = IntSet.Empty.incl(8).incl(8).incl(5)
val y = IntSet.Empty.incl(3).incl(5)
x.union(y)

x contain 6
IntSet.Empty.incl(2).union(IntSet.Empty)