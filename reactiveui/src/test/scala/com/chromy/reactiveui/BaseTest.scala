package com.chromy.reactiveui

import com.chromy.reactiveui.myjavafx.{LocalAction, Action, Uid}
import monocle.macros.GenLens
import rx.lang.scala.Observer
import rx.lang.scala.schedulers.ImmediateScheduler

/**
 * Created by cry on 2015.07.12..
 */
trait BaseTest {

  implicit var list = List[Change[_]]()


  case class Change[M <: Model[_ <: Component]](name: String, newModel: M)



  implicit class component2State[C <: Component](comp : C ) {
    def state: Option[C#ModelType] = {
      var state: C#ModelType = null.asInstanceOf[C#ModelType]
      val subscription = comp.changes.subscribeOn(ImmediateScheduler()).subscribe({lastState => state = lastState})
      subscription.unsubscribe()
      state match {
        case null => None
        case _ => Some(state)
      }
    }
    def prependToList(name: String): Unit = {
      comp.changes.subscribe({change =>
        list = Change(name, change) :: list
      })
    }
  }

  case class AddNumber(uid: String = "", number: Int) extends Action

  case class MulNumber(uid: String = "", number: Int) extends LocalAction

  case class MainModel(left: SubModel = SubModel(), right: SubModel = SubModel(), uid: String = Uid.nextUid().toString) extends Model[MainComponent]
  case class SubModel(value: Int = 0, uid: String = Uid.nextUid().toString) extends Model[SubComponent]

  case object AddItem extends Action

  class MainComponent(val parentRouter: Router[MainModel], val initialState: MainModel = MainModel()) extends BaseComponent[MainModel] {
    override def update: (Action, MainModel, Observer[Action]) => MainModel = { (action, model, channel) =>
      model
    }

    class ChildrenComponents {
      val left = new SubComponent(parentRouter.map(GenLens[MainModel](_.left)), initialState.left)
      val right = new SubComponent(parentRouter.map(GenLens[MainModel](_.right)), initialState.right)
    }
    val childrenComponents = new ChildrenComponents
  }

  class SubComponent(val parentRouter: Router[SubModel], val initialState: SubModel = SubModel()) extends BaseComponent[SubModel]  {
    override def update: (Action, SubModel, Observer[Action]) => SubModel = { (action, model, channel) =>
      action match {
        case AddNumber(model.uid, toAdd) => model.copy(value = model.value + toAdd)
        case MulNumber(model.uid, toMul) => model.copy(value = model.value * toMul)
        case _ => model
      }
    }
  }

  case class ListModel(subs: List[SubModel] = Nil, uid: String = Uid.nextUid().toString) extends Model[ListComponent]
  class ListComponent(val parentRouter: Router[ListModel], val initialState: ListModel = ListModel()) extends BaseComponent[ListModel] {
    override def update: (Action, ListModel, Observer[Action]) => ListModel = { (action, model, channel) =>
      action match {
        case AddItem =>
          val newList = SubModel() :: model.subs
          model.copy(subs = newList)
        case _ => model
      }

    }

    class ChildrenComponents {
      val lens = parentRouter.map(GenLens[ListModel](_.subs))
      val subs = ListComponentOf[SubModel](lens)(router => new SubComponent(router))
    }
    val childrenComponents = new ChildrenComponents
  }

}
