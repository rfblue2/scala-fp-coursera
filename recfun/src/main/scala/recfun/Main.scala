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
      def fact(x: Int): Int = if (x <= 0) 1 else x * fact(x - 1)

      fact(r) / (fact(c) * fact(r - c))
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(chars: List[Char], count: Int): Boolean = {

        def newcount(c: Char): Int =
          if      (c == '(') count + 1
          else if (c == ')') count - 1
          else               count

        if (chars.isEmpty) count == 0
        else if (count < 0) false // close parens without open
        else loop(chars.tail, newcount(chars.head))
      }

      loop(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def loop(money: Int, coinsPartial: List[Int]): Int = {
        if (money == 0) 1
        else if (coinsPartial.isEmpty) 0
        else if (money - coinsPartial.head >= 0)
          loop(money, coinsPartial.tail) +
            loop(money - coinsPartial.head, coinsPartial)
        else loop(money, coinsPartial.tail)
      }
      loop(money, coins)
    }
  }
