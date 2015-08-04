package com.chromy.reactiveui

import com.chromy.reactiveui.core._
import monocle.macros.GenLens
import org.scalatest.FunSpecLike
import rx.lang.scala.Observer

/**
 * Created by cry on 2015.07.05..
 */
class ComponentTest extends FunSpecLike {

  case class AddNumber(uid: Uid, number: Int) extends Action

  case class MulNumber(uid: Uid, number: Int) extends LocalAction

  case class MainModel(left: SubModel = SubModel(), right: SubModel = SubModel(), uid: Uid = Uid()) extends Model[MainComponent]

  case class SubModel(value: Int = 0, uid: Uid = Uid()) extends Model[SubComponent]

  case object AddItem extends Action

  class MainComponent(val routerMapper: RouterMapper[MainModel], val initialState: MainModel = MainModel()) extends BaseComponent[MainModel] {
    override def update: (Action, MainModel, Observer[Action]) => MainModel = { (action, model, channel) =>
      model
    }

    class ChildrenComponents {
      val left = new SubComponent(router.map(GenLens[MainModel](_.left)), initialState.left)
      val right = new SubComponent(router.map(GenLens[MainModel](_.right)), initialState.right)
    }

    val childrenComponents = new ChildrenComponents
  }

  class SubComponent(val routerMapper: RouterMapper[SubModel], val initialState: SubModel = SubModel()) extends BaseComponent[SubModel] {
    override def update: (Action, SubModel, Observer[Action]) => SubModel = { (action, model, channel) =>
      action match {
        case AddNumber(model.uid, toAdd) => model.copy(value = model.value + toAdd)
        case MulNumber(model.uid, toMul) => model.copy(value = model.value * toMul)
        case _ => model
      }
    }
  }

  case class ListModel(subs: List[SubModel] = Nil, uid: Uid = Uid()) extends Model[ListComponent]

  class ListComponent(val routerMapper: RouterMapper[ListModel], val initialState: ListModel = ListModel()) extends BaseComponent[ListModel] {
    override def update: (Action, ListModel, Observer[Action]) => ListModel = { (action, model, channel) =>
      action match {
        case AddItem =>
          val newList = SubModel() :: model.subs
          model.copy(subs = newList)
        case _ => model
      }

    }

    class ChildrenComponents {
      val subs = ListComponentOf[SubModel](router.map(GenLens[ListModel](_.subs)))(router => new SubComponent(router.mapper))
    }

    val childrenComponents = new ChildrenComponents
  }


  describe("Component") {
    ignore("should update its state by an action") {
      new BaseTest {
        val comp = TestComponent[MainComponent](MainModel()) { (mapper, initialState) => new MainComponent(mapper, initialState) }
        comp.router.channel.onNext(AddNumber(Uid(0), 10))
        assert(comp.state != None)
        assert(comp.state.get.left.value == 10)
        assert(comp.state.get.right.value == 0)
      }
    }
    ignore("should update its state by a local action") {
      new BaseTest {

        val comp = TestComponent[MainComponent](MainModel()) { (router, initialState) => new MainComponent(router, initialState) }

        comp.component.childrenComponents.left.prependToList("SubComponent.left")
        comp.component.childrenComponents.right.prependToList("SubComponent.right")

        comp.router.channel.onNext(AddNumber(Uid(0), 5))
        comp.component.childrenComponents.left.router.channel.onNext(MulNumber(Uid(0), 10))
        comp.router.channel.onNext(AddNumber(Uid(1), 5))

        println("**************************")
        println(list)
      }
    }
    ignore("should update its state by a local action, and the next global action should not reflect its state") {
      new BaseTest {
        val comp = TestComponent[MainComponent](MainModel(SubModel(5), SubModel(5))) { (router, initialState) => new MainComponent(router, initialState) }

        comp.component.childrenComponents.left.router.channel.onNext(MulNumber(Uid(0), 10))

        comp.router.channel.onNext(AddNumber(Uid(0), 5))

        assert(comp.component.childrenComponents.left.state != None)
        assert(comp.component.childrenComponents.left.state.get.value == 50)
      }
    }

    it("should be able to handle lists") {
      new BaseTest {
        val comp = TestComponent[ListComponent](ListModel()) { (mapper, initialState) => new ListComponent(mapper, initialState) }

        comp.component.childrenComponents.subs.prependToList("ListComponent.subs")
        //comp.component.childrenComponents.subs.childrenComponents(0)
        comp.component.router.channel.onNext(AddItem)
        comp.component.router.channel.onNext(AddItem)
        println(list)
      }
    }

  }
}
