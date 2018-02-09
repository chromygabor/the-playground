import scala.annotation.tailrec

object ListPlayground extends App {

  def listToListOfList[A](original: Seq[A], pageSize: Int): Seq[Seq[(A)]] = {
    val indexedSeq = original.zipWithIndex

    def loop(remainder: Seq[(A, Int)], pageNum: Int = 1, accu: Seq[Seq[(A)]] = Seq.empty): Seq[Seq[(A)]] = {
      val (ready, rem) =  splitIf(remainder)(in => in._2 / pageSize == pageNum)
      val newAccu = accu :+ ready.map(_._1)
      if(rem.isEmpty) newAccu
      else loop(rem, pageNum + 1, newAccu)
    }

    loop(indexedSeq)
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
  println(listToListOfList(l, 3).mkString("-----------------\n", "\n", ""))
}
