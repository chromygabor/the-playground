'''scala
object Basics extends App {
  val list = List("A", "B", "C", "D")

  val result = list.scanLeft("Z") { (accu, item) =>
    accu + item
  }

  println(result)
}
'''

Output:
'''console
List(Z, ZA, ZAB, ZABC, ZABCD)
'''
