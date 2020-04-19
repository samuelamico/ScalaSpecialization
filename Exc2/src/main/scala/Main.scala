object Main extends App {
  println(recfun.RecFun.pascal(1,3))

  println(recfun.RecFun.balance("(if (zero? x) max (/ 1 x))".toList))
}

