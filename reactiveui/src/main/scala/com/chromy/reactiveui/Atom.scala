package com.chromy.reactiveui

/**
 * Created by cry on 2015.05.23..
 */
object Atom {
  def apply[S](_initialValue: S)(subscriber: ((S, S)) => Unit) = new Atom[S] {
    override val initialValue: S = _initialValue
    model.subscribe(subscriber)
  }
}

trait Atom[S] {
  val initialValue: S

  private val foldFlow = Signal.mailbox[S => S]
  private lazy val mf = foldFlow.signal.scan((initialValue, initialValue)) { case ((beforePrevModel, prevModel), newModelFunc: (S => S)) =>
    val newModel = newModelFunc(prevModel)
    (prevModel, newModel)
  }
  private val changesMb = Signal.mailbox[S]

  lazy val changes = {
    changesMb.address.onNext(initialValue)
    mf.drop(1).subscribe(
    { value => changesMb.address.onNext(value._2) }, { error => changesMb.address.onError(error) }, { () => changesMb.address.onCompleted() }
    )
    changesMb.signal.distinctUntilChanged
  }
  lazy val model = mf.drop(1)

  def fire(newModel: S): Unit = {
    foldFlow.address.onNext(_ => newModel)
  }

  private def onNext(updatedModel: S => S): Unit = {
    foldFlow.address.onNext(updatedModel)
  }

  def map[B](get: S => B)(set: B => S => S): Atom[B] = {
    val parent = this
    new Atom[B] {
      override val initialValue: B = get(parent.initialValue)
      model.subscribe { in =>
        parent.onNext(set(in._2))
      }
    }
  }

}