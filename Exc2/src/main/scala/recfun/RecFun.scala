package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    val initial_value = Array(1)

    def sum(x: Int, y: Array[Int]): Int = {
      if (y.isEmpty) 1
      else x + y.head
    }

    def row_tri(x: Array[Int], y: Array[Int]): Array[Int] = {
      if (y.isEmpty) x
      else row_tri( sum(y.head,y.tail) +: x, y.tail)
    }

    def interact(row: Int, v: Array[Int]): Int = {
      if (row == 0) v.apply(c)
      else interact(row - 1, row_tri(initial_value, v))
    }

    interact(r,initial_value)

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var co = 0
    var cc = 0

    def check_parenthese(c_open: Int, c_close: Int, x: List[Char]): Boolean = {
      if (x.isEmpty) {
        if (c_open == c_close) true
        else false
      }
      else {
        if (c_open < c_close) {
          false
        }
        else {
          if (x.head == '(') check_parenthese(c_open + 1, c_close, x.tail)
          else if (x.head == ')') check_parenthese(c_open, c_close + 1, x.tail)
          else check_parenthese(c_open, c_close, x.tail)
        }
      }
    }
    check_parenthese(co,cc,chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
