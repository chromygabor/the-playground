package com.chromy.frpui

import com.chromy.frpui.core._
import com.chromy.frpui.util.DiffUtils._
import monocle.macros.GenLens
import rx.lang.scala.Subject

import scala.collection.mutable.ArrayBuffer

/**
 * Created by cry on 2015.10.15..
 */
object ConceptApp extends App {

  case object Nop extends Action
  case class IncrementValue(number: Int, uid: Uid) extends Action

  case class DecrementValue(number: Int, uid: Uid) extends Action

  case class AddItem(uid: Uid) extends Action

  case class AddValue(name: Uid) extends Action

  case class LeftSubmodel(value: Int = 0, uid: Uid = Uid()) extends Model[LeftSubmodel] {
    override def handle(implicit context: Context): Updater[LeftSubmodel] = Updater {
      case Init => this
      case IncrementValue(number, this.uid) => copy(value = value + number)
      case DecrementValue(number, this.uid) => copy(value = value + number)
    }
     
  }

  case class RightSubmodel(value: Int = 0, uid: Uid = Uid()) extends Model[RightSubmodel] {
    override def handle(implicit context: Context): Updater[RightSubmodel] = Updater {
      case Init => this
      case IncrementValue(number, this.uid) => copy(value = value + number)
      case DecrementValue(number, this.uid) => copy(value = value + number)
    }
    
  }

  case class MainModel(left: List[LeftSubmodel] = Nil, right: Map[Uid, RightSubmodel] = Map(), uid: Uid = Uid()) extends Model[MainModel] {
    override def children = MainModel.children

    override def handle(implicit context: Context): Updater[MainModel] = Updater {
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
    val rightComponenets = scala.collection.mutable.Map[Uid, RightSubComponent]()

    val subscriber = Render.scan[MainModel](initialState) { (prev, model) =>
      /*
      Rendering left as a List
      */
      val left = prev.left.diffOps(model.left)(_.uid).map {
        case Insert(pos, item) =>
          val component = LeftSubComponent(render.map(_.left).indexOption(pos), item)
          Insert(pos, component)
        case Remove(pos) => Remove(pos)
        case Move(from, to) => Move(from, to)
      }

       /*
       Rendering right as Map
       */
       val right = prev.right.diffOps(model.right) map {
          case KeyUpdate(key, value) =>
            val newComponent = RightSubComponent(render.map(_.right).keyOption(key: Uid), value)
            KeyUpdate[Uid, RightSubComponent](key, newComponent)
          case KeyRemove(key) =>
            KeyRemove[Uid, RightSubComponent](key: Uid)
       }


      SideEffect {
        println(s"[MainComponent] $prev => $model")

        left.foreach {
          case Insert(pos, component) =>
            leftComponents.insert(pos, component)
            component.render.update(component.initialState).run()
          case Remove(pos) =>
            leftComponents.remove(pos)
          case Move(from, to) =>
            val component = leftComponents.remove(from)
            leftComponents.insert(to, component)
        }

        right.foreach {
          case KeyUpdate(key, component) =>
            rightComponenets.update(key, component)
            component.render.update(component.initialState).run()
          case KeyRemove(key) =>
            rightComponenets.remove(key)
        }
      }
    }
    render.distinctUntilChanged.subscribe(subscriber)
  }

  case class LeftSubComponent(render: SideEffectChain[LeftSubmodel], initialState: LeftSubmodel) extends Component[LeftSubmodel] {
    val subscriber = Render[LeftSubmodel] { model =>
      SideEffect {
        println(s"[LeftSubComponent#${initialState.uid}] $model" )
      }
    }

    render.distinctUntilChanged.subscribe(subscriber)
  }

  case class RightSubComponent(render: SideEffectChain[RightSubmodel], initialState: RightSubmodel) extends Component[RightSubmodel] {
    val subscriber = Render[RightSubmodel] {model =>
      SideEffect {
        println(s"[RightSubComponent#${initialState.uid}] $model")
      }
    }
    render.distinctUntilChanged.subscribe(subscriber)
  }

  val s = Subject[Action]
  val initialModel = MainModel()
  val render = SideEffectChain[MainModel]()

  val stream = s.scan(initialModel) { (model, action) =>
    println(s"================== Action received: $action")
    model.step(action)(new Context {
      override def getService[B : Manifest]: B = ???

      override def onAction(action: Action): Unit = s.onNext(action)
    })
  }.subscribe({ model =>
    render.update(model).run()
  })

  val m = MainComponent(render, initialModel)

  val u1 = Uid()
  s.onNext(AddItem(u1))
//  s.onNext(IncrementValue(2, u1))
//  s.onNext(IncrementValue(5, u1))

  val u2 = Uid()
  s.onNext(AddValue(u2))
//  s.onNext(IncrementValue(7, u2))
//  s.onNext(IncrementValue(9, u2))
//  s.onNext(IncrementValue(11, u2))

}
