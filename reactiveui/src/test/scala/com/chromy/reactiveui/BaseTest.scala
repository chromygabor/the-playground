package com.chromy.reactiveui

import com.chromy.reactiveui.myjavafx.{Action, Uid}
import monocle.macros.GenLens
import rx.lang.scala.Observer

/**
 * Created by cry on 2015.07.12..
 */
trait BaseTest {

  case class Add(number: Int) extends Action

  case class Mul(number: Int) extends Action

  case class MainModel(left: SubModel = SubModel(), right: SubModel = SubModel(), uid: String = Uid.nextUid().toString) extends Model[MainComponent]
  case class ListModel(subs: List[SubModel] = Nil, uid: String = Uid.nextUid().toString) extends Model[ListComponent]
  case class SubModel(value: Int = 0, uid: String = Uid.nextUid().toString) extends Model[SubComponent]


  class MainComponent(val parentRouter: Router[MainModel], val initialState: MainModel = MainModel()) extends BaseComponent[MainModel] {
    override def update: (Action, MainModel, Observer[Action]) => MainModel = { (action, model, channel) =>
      model
    }

    val children = new {
      val left = changes.map(_.left)
      val right = changes.map(_.right)
    }

    val subcomponents = Map(
      "left" ->   new SubComponent(parentRouter.map(GenLens[MainModel](_.left))),
      "right" ->  new SubComponent(parentRouter.map(GenLens[MainModel](_.right)))
    )
  }
  class SubComponent(val parentRouter: Router[SubModel], val initialState: SubModel = SubModel()) extends BaseComponent[SubModel]  {
    override def update: (Action, SubModel, Observer[Action]) => SubModel = { (action, model, channel) =>
      action match {
        case Add(toAdd) => model.copy(value = model.value + toAdd)
        case Mul(toMul) => model.copy(value = model.value * toMul)
      }
    }

    val children = new {
      val value = changes.map(_.value)
    }
  }


  class ListComponent(val parentRouter: Router[ListModel], val initialState: ListModel = ListModel()) extends BaseComponent[ListModel] {

    override def update: (Action, ListModel, Observer[Action]) => ListModel = { (action, model, channel) =>
      model
    }

    val subcomponents = Map(
      "subs" ->   new ListComponentOf[SubModel](parentRouter.map(GenLens[ListModel](_.subs)))({router => new SubComponent(router)})
        )
  }

}
