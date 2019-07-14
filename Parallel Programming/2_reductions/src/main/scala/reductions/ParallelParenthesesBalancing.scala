package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    val np = chars.foldLeft(0)((n, c) =>
      c match {
        case '(' => if (n >= 0) n + 1 else -1
        case ')' => if (n > 0) n - 1 else -1
        case _   => n
      })
    np == 0
  }

  type IntPair = (Int, Int)

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int): IntPair /*: ???*/ = {
      /*
      ((( 3/0
      (() 1/0
      ()( 1/0
      )(( 2/-1
      ))( 1/-2
      )() 0/-1
      ()) 0/-1
      ))) 0/-3
       */
      chars
        .slice(from, until)
        .foldLeft((0, 0))((p, c) =>
          c match {
            case '(' => if (p._1 >= 0) (p._1 + 1, p._2) else (p._1, p._2 + 1)
            case ')' => if (p._1 > 0) (p._1 - 1, p._2) else (p._1, p._2 - 1)
            case _   => (p._1, p._2)
          })
    }

    def reduce(from: Int, until: Int): IntPair /*: ???*/ = {

      val L = until - from
      if (L <= 0) (0, 0)
      else if (L <= threshold) traverse(from, until)
      else {

        val a = reduce(from, from + L / 2)
        val b = reduce(from + L / 2, until)

        println(a)

        val mix = a._1 + b._2

        if (mix > 0) (mix + b._1, a._2)
        else (b._1, mix + a._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
}
