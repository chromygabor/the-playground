import scala.annotation.tailrec

object ListPlayground extends App {

  def listToListOfList[A](original: Seq[A], pageSize: Int): Seq[Seq[A]] = {
    val indexedSeq = original.zipWithIndex
    val size = original.size

    def loop(remainder: Seq[(A, Int)], pageNum: Int = 0, accu: Seq[(A, Int)]) = {
      val (ready, rem) =  splitIf(remainder)(in => in._2 / pageSize == pageNum)
      ready
    }

    println(splitIf(indexedSeq)(in => in._2 / pageSize == 1))

    ???
  }

  @tailrec
  def splitIf[A](remainder: Seq[A], accu: Seq[A] = Seq.empty)(f: A => Boolean): (Seq[A], Seq[A]) = {
    if(remainder.nonEmpty && !f(remainder.head)) {
      splitIf(remainder.tail, accu :+ remainder.head)(f)
    } else
      (accu, remainder)
  }

  val l = (1 to 10).toList

  println(l)
  println(listToListOfList(l, 3))
}
