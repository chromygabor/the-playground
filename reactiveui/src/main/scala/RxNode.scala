package rx.lang.scala

import scala.language.implicitConversions

/**
 * Created by chrogab on 2015.04.10..
 */
trait Event

trait RxNode extends Observer[Event] {
  private[rx] val receiver = Subject[Event]()
  private[rx] val out = Subject[Event]

  override final def onNext(event: Event) = receiver.onNext(event)

  override final def onError(error: Throwable) = receiver.onError(error)

  override final def onCompleted() = receiver.onCompleted()

  protected val reactors = new RxNodeReactors(this)

  def tell(event: Event) = onNext(event)
  protected def publish(event: Event) = out.onNext(event)

  protected def from[T : Manifest](): Observable[T] = {
    receiver.
      filter(event => event.getClass == manifest[T].runtimeClass).
      map(event => event.asInstanceOf[T])
  }

}

class RxNodeReactors(node: RxNode) {
  def +=(reactor: PartialFunction[Event, Unit]) = {
    node.receiver.subscribe(event => reactor.apply(event))
  }
}

object RxNode {
  implicit def rxNodeToObservable(rxNode: RxNode): Observable[Event] = rxNode.out.asInstanceOf[Observable[Event]]
}