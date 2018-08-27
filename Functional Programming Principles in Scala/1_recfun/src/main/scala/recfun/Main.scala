package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c==r || c==0 || r==0) 1 else pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def go(bal: Int, remain: List[Char]): Int = {
        if (remain.isEmpty || bal<0) bal
        else if (remain.head == '(') go(bal + 1, remain.tail)
        else if (remain.head == ')') go(bal - 1, remain.tail)
        else go(bal, remain.tail)
      }
      go(0, chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money<=0 || coins.isEmpty) 0
      else if (coins.head == money) 1 + countChange(money, coins.tail)
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
