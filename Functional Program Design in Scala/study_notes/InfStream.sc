def from(n: Int): Stream[Int] = n #:: from(n+1)
def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail filter (_ % s.head != 0 ))
}
val nat = from(0)
val m4s = nat map(_*4)

val primes = sieve(from(2))

(primes take 100).toList

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double): Double = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

def isGoodEnough(guess: Double, x: Double) = {
  math.abs((guess*guess - x) / x ) < 0.001
}

sqrtStream(3).take(5).toList
sqrtStream(3).filter(isGoodEnough(_, 3)).take(5).toList