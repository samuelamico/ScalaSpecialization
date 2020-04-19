import java.security.KeyStore.TrustedCertificateEntry

import com.sun.org.apache.xpath.internal.functions.FuncFalse

/*

Pascal Draft

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

 */

// Parentheses Balancing
var k = "() )(".toList


var co = 0
var cc = 0

def check_parenthese(c_open: Int, c_close: Int, x: List[Char]): Boolean = {
  if (x.isEmpty) {
    if (c_open == c_close) true
    else false
  }
  else {
    if (c_open < c_close ){
        false
    }
    else{
      if (x.head == '(') check_parenthese(c_open+1,c_close,x.tail)
      else if (x.head == ')') check_parenthese(c_open,c_close+1,x.tail)
      else check_parenthese(c_open,c_close,x.tail)
    }
  }
}

check_parenthese(co,cc,k)


 /*


def check(x: List[Char]): Unit = {
  println(x)
  if(x.isEmpty) println("FIM")
  else {
    if(x.head == '(') check(x.tail)
    else check(x.tail)
  }

}

check(k)

 */