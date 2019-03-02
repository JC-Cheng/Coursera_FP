package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def solSet(A: Double, B: Double, C: Double, D: Double): List[Double] = {
      if (D < 0) Nil
      else if (D > 0) {
        val sqrtD = math.pow(D, 0.5)
        (-B+sqrtD) / (2*A) :: (-B-sqrtD) / (2*A) :: Nil
      } else -B / (2*A) :: Nil
    }
    Signal(solSet(a(), b(), c(), delta()).toSet)
  }
}
