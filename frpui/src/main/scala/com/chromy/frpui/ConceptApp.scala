package com.chromy.frpui

import com.chromy.frpui.util.ListDiff
import ListDiff.{ListOp, Move, Remove, Insert}
import com.chromy.frpui.core.Updater.Simple
import com.chromy.frpui.core._
import monocle.macros.GenLens
import rx.lang.scala.Subject

import scala.collection.mutable.ArrayBuffer

/**
 * Created by cry on 2015.10.15..
 */
object ConceptApp extends App {

  case class IncrementValue(number: Int, uid: Uid) extends Action

  case class DecrementValue(number: Int, uid: Uid) extends Action

  case class AddItem(uid: Uid) extends Action

  case class AddValue(name: Uid) extends Action

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

  case class MainComponent(render: SideEffectChain[MainModel], initialState: MainModel = MainModel()) extends Component[MainModel] {
    val leftComponents = ArrayBuffer[LeftSubComponent]()

    val subscriber = Render.scan[MainModel](initialState) { (prev, model) =>
      val l = ListDiff.diffOps(prev.left, model.left)(_.uid.uid).map {
        case Insert(pos, item) =>
          val component = LeftSubComponent(render.map(_.left).indexOption(pos), item)
          Insert(pos, component)
        case Remove(pos) => Remove(pos)
        case Move(from, to) => Move(from, to)
      }
      SideEffect {
        println(s"[MainComponent] $prev => $model")
        l.foreach {
          case Insert(pos, component) =>
            leftComponents.insert(pos, component)
            component.render.update(component.initialState).run()
          case Remove(pos) =>
            leftComponents.remove(pos)
          case Move(from, to) =>
            val component = leftComponents.remove(from)
            leftComponents.insert(to, component)
        }
      }
    }
    render.subscribe(subscriber)
  }

  case class LeftSubComponent(render: SideEffectChain[LeftSubmodel], initialState: LeftSubmodel) extends Component[LeftSubmodel] {
    val subscriber = Render[LeftSubmodel] { model =>
      SideEffect {
        println(s"[LeftSubComponent] $model" )
      }
    }

    render.subscribe(subscriber)
  }
  case class RightSubComponent(render: SideEffectChain[RightSubmodel]) extends Component[RightSubmodel]

  val s = Subject[Action]
  val initialModel = MainModel()
  val render = SideEffectChain[MainModel]()

  val stream = s.scan(initialModel) { (model, action) =>
    model.step(action)
  }.subscribe({ model =>
    render.update(model).run()
  })

  val m = MainComponent(render, initialModel)

  val u1 = Uid()
  s.onNext(AddItem(u1))
  s.onNext(IncrementValue(10, u1))
  s.onNext(IncrementValue(10, u1))

}
