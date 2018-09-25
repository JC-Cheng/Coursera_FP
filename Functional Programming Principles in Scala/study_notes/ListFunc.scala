
object ListFunc extends App{

  // sorting
  def msort[T](Xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = Xs.length / 2
    if (n == 0) Xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x::xs1, y::ys1) => if(lt(x, y)) x::merge(xs1, ys) else y::merge(xs, ys1)
      }
      val (fst, snd) = Xs.splitAt(n)
      merge(msort(fst)(lt), msort(snd)(lt))
    }
  }

  val nums = List(4, 3, 6, 1, -4, 11, -1, 3, 5)
  println(msort(nums)((x, y) => x < y))

  val fruit = List("apple", "grape", "banana", "durian", "pineapple", "kiwi")
  println(msort(fruit)((x, y) => x.compareTo(y) < 0 ))

  // filter
  println(nums.filter(x => x % 2 == 0))
  println(nums.filterNot(x => x % 2 == 0))
  println(nums.partition(x => x % 2 == 0))

  println(fruit.takeWhile(s => s.length < 6))
  println(fruit.dropWhile(s => s.length < 6))
  println(fruit.span(s => s.length < 6))

  def pack[U](xs: List[U]): List[List[U]] = xs match {
    case Nil => List()
    case x::_ => {
      val (fst, rest) = xs.span(y => y == x)
      fst :: pack(rest)
    }
  }

  def encode[U](xs: List[U]): List[(U, Int)] = pack(xs).map(l => (l.head, l.length))

  val text = "Keep your friends close, but your enemies closer."
  val vowels = text.toList.filter(p => Set('a', 'e', 'i', 'o', 'u').contains(p))
  println(vowels)
  println(pack(vowels))
  println(encode(vowels))

  // reduce

  println(nums.reduceLeft(_ + _), nums.foldLeft(0)(_ + _))
  println(nums.reduceLeft(_ * _), nums.foldLeft(1)(_ * _))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(f(_) :: _ )

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((_: T, n: Int) => n + 1)

  println(mapFun(nums, (x: Int) => x + 0.01))
  println(lengthFun(nums))
}
