import scala.io.Source

object nqueens {
  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k==0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col::queens
    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {case (r, c) => col != c && math.abs(col - c) != row - r}
  }

  def show(queens: List[Int]): String = {
    val lines = for (col <- queens.reverse) yield Vector.fill(queens.length)("* ").updated(col, "Q ").mkString
    "\n" + (lines mkString "\n")
  }
}

class Poly(val t: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = t withDefaultValue 0.0
  def eval(x: Double): Double = terms.map(t => t._2 * math.pow(x, t._1)).sum
  def + (other: Poly) = new Poly(other.terms.foldLeft(terms)(addTerm))
  def addTerm(t: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = t + (term._1 -> (term._2 + terms(term._1)))
  override def toString = (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "X^" + exp).mkString(" + ")
}


object mnemonics {

  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
  val words = in.getLines.toList.filter(word => word.forall(chr => chr.isLetter))
  val mnem = Map('2'-> "ABC", '3'->"DEF", '4'->"GHI", '5'->"JKL", '6'->"MNO", '7'->"PQRS", '8'->"TUV", '9'->"WXYZ")
  val charCode: Map[Char, Char] = for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit
  def wordCode(w: String): String = w.toUpperCase.map(charCode)

  val wordsForNum: Map[String, Seq[String]] = words.groupBy(wordCode).withDefaultValue(Seq())
  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number.take(split))
        rest <- encode(number.drop(split))
      } yield word::rest
    }.toSet
  }

  def translate(number: String): Set[String] = encode(number).map(_ mkString " ")
}

object Example extends App{

  println(mnemonics.words.take(10))
  println(mnemonics.encode("7225247386"))

  val sol = nqueens.queens(4).map(nqueens.show)
  println(sol.mkString("\n"))
  println(sol.size)

  val p1 = new Poly(1->2.0, 3->3.0, 5->0.2)
  val p2 = new Poly(Map(0->1.0, 3->2.0))

  println(math.pow(5, 3))
  println(p1 + p2)
  println((p1 + p2).eval(0))
  println((p1 + p2).eval(-1))
  println((p1 + p2).eval(2))

}

