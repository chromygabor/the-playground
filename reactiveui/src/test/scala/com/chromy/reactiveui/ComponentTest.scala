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

  class MainComponent(val contextMapper: ContextMapper[MainModel], val initialState: MainModel = MainModel()) extends Component[MainModel] {
    override protected def upd(model: MainModel): PartialFunction[Action, MainModel] = {case _ => model}

    class ChildrenComponents {
      val left = new SubComponent(context.map(GenLens[MainModel](_.left)), initialState.left)
      val right = new SubComponent(context.map(GenLens[MainModel](_.right)), initialState.right)
    }

    val childrenComponents = new ChildrenComponents

  }

  class SubComponent(val contextMapper: ContextMapper[SubModel], val initialState: SubModel = SubModel()) extends Component[SubModel] {
    override protected def upd(model: SubModel): PartialFunction[Action, SubModel] = {
      case AddNumber(model.uid, toAdd) => model.copy(value = model.value + toAdd)
      case MulNumber(model.uid, toMul) => model.copy(value = model.value * toMul)
      case _ => model
    }
  }

  case class ListModel(subs: List[SubModel] = Nil, uid: Uid = Uid()) extends Model[ListComponent]

  class ListComponent(val contextMapper: ContextMapper[ListModel], val initialState: ListModel = ListModel()) extends Component[ListModel] {
    override protected def upd(model: ListModel): PartialFunction[Action, ListModel] = {
      case AddItem =>
        val newList = SubModel() :: model.subs
        model.copy(subs = newList)
      case _ => model
    }

    class ChildrenComponents {
      //val subs = ListComponentOf[SubModel](router.map(GenLens[ListModel](_.subs)))(router => new SubComponent(router.mapper))
    }

    val childrenComponents = new ChildrenComponents

  }


  describe("Component") {
    ignore("should update its state by an action") {
      new BaseTest {
        val comp = TestComponent[MainComponent](MainModel()) { (mapper, initialState) => new MainComponent(mapper, initialState) }
        comp.context.channel.onNext(AddNumber(Uid(0), 10))
        assert(comp.state != None)
        assert(comp.state.get.left.value == 10)
        assert(comp.state.get.right.value == 0)
      }
    }
    ignore("should update its state by a local action") {
      new BaseTest {

        val comp = TestComponent[MainComponent](MainModel()) { (context, initialState) => new MainComponent(context, initialState) }

        comp.component.childrenComponents.left.prependToList("SubComponent.left")
        comp.component.childrenComponents.right.prependToList("SubComponent.right")

        comp.context.channel.onNext(AddNumber(Uid(0), 5))
        comp.component.childrenComponents.left.context.channel.onNext(MulNumber(Uid(0), 10))
        comp.context.channel.onNext(AddNumber(Uid(1), 5))

        println("**************************")
        println(list)
      }
    }
    ignore("should update its state by a local action, and the next global action should not reflect its state") {
      new BaseTest {
        val comp = TestComponent[MainComponent](MainModel(SubModel(5), SubModel(5))) { (context, initialState) => new MainComponent(context, initialState) }

        comp.component.childrenComponents.left.context.channel.onNext(MulNumber(Uid(0), 10))

        comp.context.channel.onNext(AddNumber(Uid(0), 5))

        assert(comp.component.childrenComponents.left.state != None)
        assert(comp.component.childrenComponents.left.state.get.value == 50)
      }
    }

    it("should be able to handle lists") {
      new BaseTest {
//        val comp = TestComponent[ListComponent](ListModel()) { (mapper, initialState) => new ListComponent(mapper, initialState) }
//
//        comp.component.childrenComponents.subs.prependToList("ListComponent.subs")
//        //comp.component.childrenComponents.subs.childrenComponents(0)
//        comp.component.router.channel.onNext(AddItem)
//        comp.component.router.channel.onNext(AddItem)
//        println(list)
      }
    }

  }
}
