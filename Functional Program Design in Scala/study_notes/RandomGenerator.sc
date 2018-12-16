
trait Generator[+T] {
  self =>

  def generate: T //function

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt
}

val booleans = for (x <- integers) yield x > 0
val b = booleans.generate

def pairs[T, U](t: Generator[T], u: Generator[U]) = t flatMap { x =>
  u map { y =>
    (x, y)
  }
}

val y = pairs(integers, booleans).generate

def single[T](x: T): Generator[T] = new Generator[T] { def generate = x }
def choose(lo: Int, hi: Int): Generator[Int] = {
  for (x <- integers) yield lo + x % (hi - lo)
}
def oneof[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)

def lists: Generator[List[Int]] =
  for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyList else nonEmptyLists
  } yield list

def emptyList = single(Nil)
def nonEmptyLists = for { head <- integers; tail <- lists } yield head :: tail
val l = lists.generate

trait Tree[T]
case class Inner[T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T](x: T) extends Tree[T]

def bLeaves: Generator[Leaf[Boolean]] = for (x<-booleans) yield Leaf(x)
def bInners: Generator[Inner[Boolean]] = for (l<-bTrees; r<-bTrees) yield Inner(l, r)
def bTrees: Generator[Tree[Boolean]] = for {
  isLeaf <- booleans
  tree <- if (isLeaf) bLeaves else bInners
} yield tree

bTrees.generate
