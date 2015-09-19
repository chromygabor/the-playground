/**
 * Created by cry on 2015.09.16..
 */
object StackableTraits extends App {

  trait Dispatchable[A]{
    def dispatcher(in: A): A = {
      dispatch(in)
    }
    protected def dispatch(in: A): A
  }

  trait Dispatcher1[A] extends Dispatchable[A] {
    abstract override def dispatcher(in: A): A = {
      println("Dispatcher1")
      super.dispatcher(in)
    }
  }

  trait Dispatcher2[A] extends Dispatchable[A] {
    abstract override def dispatcher(in: A): A = {
      println("Dispatcher2")
      super.dispatcher(in)
    }
  }

  class MyDispatcher extends Dispatchable[Int] with Dispatcher1[Int] with Dispatcher2[Int] {
    override def dispatch(in: Int): Int = {
      println(s"MyDispatcher: $in")
      in
    }
  }

  val myd = new MyDispatcher
  myd.dispatcher(1)

}
