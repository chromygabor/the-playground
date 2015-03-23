/**
 * Created by cry on 2015.02.04..
 */
object Fibonacci extends App {
  def fibNaive(in: Int): Int = {
    in match {
      case n if n <= 1 => n
      case n =>
        val l = fibNaive(n - 1)
        val r = fibNaive(n - 2)
        l + r
    }
  }

  type Memo = Map[Int, Int]

  def fibmemo1(in1: Int) = {

    def fibmemoR(in: Int, memo: Memo): (Int, Memo) = {
      in match {
        case n if n <= 1 => (n, memo)
        case n =>
          memo.get(n) match {
            case Some(x) => (x, memo)
            case None =>
              val (r, memo0) = fibmemoR(n-1, memo)
              val (l, memo1) = fibmemoR(n-2, memo0)
              (l + r, memo1)
          }
      }
    }
    fibmemoR(in1, Map())._1
  }

  //println(fibmemo1(10))

  case class State[S, A](run: S => (A, S)) {
    def map[B]( f: A => B): State[S, B] = State {s =>
      val (a,t) = run(s)
      (f(a),t)
    }
    def flatMap[B](f: A => State[S, B]): State[S, B] = State{s =>
      val (a,t) = run(s)
      f(a) run t
    }

    def eval(s: S) = run(s)._1
  }

  object State {
    def insert[S, A](a: A): State[S, A] = State { s=> (a, s) }

    def get[S,A](f: S => A): State[S, A] =
      State(s => (f(s), s))

    def mod[S](f: S => S): State[S, Unit] =
      State(s => ((), f(s)))
  }

  def fibmemoS(in1: Int) = {
    def fibmemoRS(in: Int): State[Memo, Int] =
      in match {
        case n if n <= 1 => State.insert(n)
        case n =>
          for {
            u <- State.get((m: Memo) => m get n)
            v <- u map State.insert[Memo, Int] getOrElse
              fibmemoRS(n - 1) flatMap {r =>
              fibmemoRS(n - 2) flatMap {s =>
                val t = r + s
                State.mod((m: Memo) => m + ((n, t))) map (_ => t)
              }}
          } yield v
    }
    fibmemoRS(in1) run Map()
  }

  println(fibmemoS(5))

  val r = State.insert(1).map { in => println(in)}

  //r run Map()
}
