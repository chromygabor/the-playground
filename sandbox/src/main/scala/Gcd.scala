import scala.annotation.tailrec

/**
 * Created by cry on 2015.01.15..
 */
object Gcd extends App{

  /**
   * Print the greatest common division of two parameters
   * It defines an inner loop, which returns the gcd of the parameters. The method complexity is O(n2)
   * @param a
   * @param b
   */
  def gcd(a: Int, b:Int): Unit = {
    print(s"input: $a,$b: ")
    @tailrec
    def gcdLoop(a: Int, b:Int) : Int = {
      print(s"($a,$b) -> ")
      b match {
        case 0 => a
        case _ => gcdLoop(b, a % b)
      }
    }
    val r = gcdLoop(a,b)
    println("result: " + r)
  }

  gcd(1, 1)
  gcd(1, 2)
  gcd(1, 3)
  gcd(1, 4)
  gcd(2, 1)
  gcd(2, 2)
  gcd(7, 6)
  gcd(11525, 564)
  gcd(564, 11525)

}
