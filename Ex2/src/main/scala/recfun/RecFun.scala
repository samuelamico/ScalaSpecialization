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
  def balance(chars: List[Char]): Boolean = ???

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
