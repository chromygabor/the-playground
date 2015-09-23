import scala.collection.mutable.{HashMap => MMap, WeakHashMap => WMap}


/**
 * Created by cry on 2015.09.22..
 */
object Chainable extends App {

  /**
   * Created by cry on 2015.07.04..
   */
  trait SideChain[T] {
    private[this] val _subscribersLock: Object = new Object()
    private[this] val _subscribers: WMap[(T => () => Unit), Int] = WMap()

    private[this] def subscribers: List[(T => () => Unit)] = {
      var newSubscribers:List[T => () => Unit]  = null
      _subscribersLock.synchronized {
        newSubscribers = _subscribers.toList.sortBy(_._2).map {
          _._1
        }
      }
      newSubscribers
    }

//    def map[B](lens: Lens[T, B]): UpdateChain[B] = {
//      val parent = UpdateChain.this
//      new UpdateChain[B] {
//
////        private val updater : (Action, T, T) => T = { (action, originalModel, model) =>
////          val newModel = this.innerUpdate(action, lens.get(originalModel), lens.get(model))
////          val res = lens.set(newModel)
////          res(model)
////        }
////
////        parent.subscribe(updater)
//      }
//    }


    val update: T => (() => Unit) = { in =>
      val allSubscribers = subscribers
      val res = { () => allSubscribers.foreach( _.apply(in)) }
      res
    }

    def subscribe(subscriber: (T) => () => Unit): Unit = {
      _subscribersLock.synchronized {
        _subscribers.update(subscriber, _subscribers.size)
      }
    }

  }

  object SideChain {
    def apply[T](): SideChain[T] = ???
  }

}
