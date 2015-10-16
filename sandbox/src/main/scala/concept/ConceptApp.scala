package concept

import java.util.concurrent.atomic.AtomicInteger

import concept.Actions._
import concept.Updater.{Simple, Updater}
import monocle.Lens
import monocle.macros.GenLens
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observable, Subject}

/**
 * Created by cry on 2015.10.15..
 */

object Actions {

  trait Action

  case class IncrementValue(number: Int, uid: Uid) extends Action

  case class DecrementValue(number: Int, uid: Uid) extends Action

  case class AddItem(uid: Uid) extends Action

  case class AddValue(name: Uid) extends Action

}

object Uid {
  val _nextUid = new AtomicInteger(0)

  private def next(): Int = _nextUid.getAndIncrement()
}

case class Uid(uid: Int = Uid.next())


object Updater {

  trait Updater[B] {
    val handle: PartialFunction[Action, B]
  }

  case class Simple[B](handle: PartialFunction[Action, B]) extends Updater[B]

}


object ConceptApp extends App {

  case class Child[M, B](lens: Lens[M, B]) {
    private[this] def updateSeq[B](action: Action, list: Seq[B]): Seq[B] = {
      list.map {
        case e: BaseModel => e.step(action).asInstanceOf[B]
        case e => e
      }
    }

    private[this] def updateMap[K, B](action: Action, map: Map[K, B]): Map[K, B] = {
      map.map {
        case (key, value: BaseModel) => (key, value.step(action).asInstanceOf[B])
        case e => e
      }
    }

    def step(action: Action, model: M): M = {
      val newModel = lens.get(model) match {
        case m: BaseModel =>
          m.step(action)
        case s: Seq[B] =>
          updateSeq(action, s)
        case m: Map[_, B] =>
          updateMap(action, m)
        case e => e
      }
      lens.set(newModel.asInstanceOf[B])(model)
    }
  }

  object BaseModel {
    def step[A <: BaseModel](action: Action, model: A, children: List[Child[A, _]], handle: Updater[A]): A = {
      val handler = handle.handle
      val initialModel: A = if (handler.isDefinedAt(action)) handler.apply(action) else model.asInstanceOf[A]
      val newModel = children.foldLeft(initialModel) { (model, child) =>
        child.step(action, model)
      }
      newModel.asInstanceOf[A]
    }
  }

  trait BaseModel {
    type M <: BaseModel

    val uid: Uid

    def children: List[Child[M, _]] = Nil

    def step(action: Action): M = BaseModel.step[M](action, this.asInstanceOf[M], children, handle)

    val handle: Updater[M] = Simple { case _ => this.asInstanceOf[M] }
  }

  trait Model[B <: BaseModel] extends BaseModel {
    type M = B
  }


  case class LeftSubmodel(value: Int = 0, uid: Uid = Uid()) extends Model[LeftSubmodel] {
    override val handle = Simple {
      case IncrementValue(number, this.uid) => copy(value = value + number)
      case DecrementValue(number, this.uid) => copy(value = value + number)
    }
  }

  case class RightSubmodel(value: Int = 0, uid: Uid = Uid()) extends Model[RightSubmodel] {
    override val handle = Simple {
      case IncrementValue(number, this.uid) => copy(value = value + number)
      case DecrementValue(number, this.uid) => copy(value = value + number)
    }
  }

  case class MainModel(left: List[LeftSubmodel] = Nil, right: Map[Uid, RightSubmodel] = Map(), uid: Uid = Uid()) extends Model[MainModel] {
    override def children = MainModel.children

    override val handle = Simple {
      case AddItem(uid) => copy(left = LeftSubmodel(uid = uid) :: left)
      case AddValue(uid) => copy(right = right.updated(uid, RightSubmodel(uid = uid)))
    }
  }

  object MainModel {
    val children = List(
      Child(GenLens[MainModel](_.left)),
      Child(GenLens[MainModel](_.right))
    )
  }

  trait BaseComponent {
    type M <: BaseModel
  }

  trait Component[A <: BaseModel] extends BaseComponent {
    type M = A
  }

  case class MainComponent(obs: Observable[MainModel]) extends Component[MainModel] {
    val parent = obs.scan((MainModel(), MainModel())) { case ((_, prevModel), actual) => (prevModel, actual) }.drop(1)
    parent.subscribe({ in => in match {
      case (prevModel, model) =>
        println(s"[MainComponent] $model")
    }
    })

  }

  case class LeftSubComponent(obs: Observable[LeftSubmodel]) extends Component[LeftSubmodel] {
    obs.subscribe({ model =>
      println(s"[LeftSubComponent] $model")
    })

  }

  case class RightSubComponent(obs: Observable[RightSubmodel]) extends Component[RightSubmodel] {

  }

  def behavior[A](observable: Observable[A]): Observable[A] = {
    val behavior = BehaviorSubject[A]

    observable.subscribe(behavior)

    behavior: Observable[A]
  }

  val s = Subject[Action]

  val stream = behavior(s.scan(MainModel()) { (model, action) =>
    model.step(action)
  })
  s.subscribe({ model => })

  val m = MainComponent(stream)

  val u1 = Uid()
  s.onNext(AddItem(u1))
  s.onNext(IncrementValue(10, u1))

}
