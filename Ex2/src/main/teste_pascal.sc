def sum(x: Int, y: Array[Int]): Int = {
  if (y.isEmpty) 1
  else x + y.head
}

def row_tri(x: Array[Int], y: Array[Int]): Array[Int] = {
  if (y.isEmpty) x
  else row_tri( sum(y.head,y.tail) +: x, y.tail)
}


var vector = Array(1,2,1)
var x = Array(1)
row_tri(x,vector)
