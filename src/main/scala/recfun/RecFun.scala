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

  pascal(0, 2)

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], nivel: Int): Boolean = chars match {
      case '(' :: t => loop(t, nivel+1)
      case ')' :: t => if (nivel == 0)
        false else loop(t, nivel-1)
      case h :: t => loop(t, nivel)
      case List() => nivel == 0
    }

    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = coins match {
    case _ if money == 0 => 1
    case h :: t if money > 0 => countChange(money-h, h :: t) + countChange(money, t)
    case _ => 0
  }
}
