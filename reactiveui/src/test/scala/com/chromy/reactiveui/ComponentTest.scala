package com.chromy.reactiveui

import org.scalatest.FunSpecLike

/**
* Created by cry on 2015.07.05..
*/
class ComponentTest extends FunSpecLike {


  describe("Component") {
    it("should update its state by an action") {
      new BaseTest {
        val comp = TestComponent[MainComponent](MainModel()){(mapper, initialState) => new MainComponent(mapper, initialState)}
        comp.channel.onNext(AddNumber("0", 10))
        assert(comp.state != None)
        assert(comp.state.get.left.value == 10)
        assert(comp.state.get.right.value == 0)
      }
    }
    ignore("should update its state by a local action") {
      new BaseTest {

        val comp = TestComponent[MainComponent](MainModel()){(router, initialState) => new MainComponent(router, initialState)}

        comp.component.childrenComponents.left.prependToList("SubComponent.left")
        comp.component.childrenComponents.right.prependToList("SubComponent.right")

        comp.channel.onNext(AddNumber("0", 5))
        comp.component.childrenComponents.left.channel.onNext(MulNumber("0", 10))
        comp.channel.onNext(AddNumber("1", 5))

        println("**************************")
        println(list)
      }
    }
    ignore("should update its state by a local action, and the next global action should not reflect its state") {
      new BaseTest {
        val comp = TestComponent[MainComponent](MainModel(SubModel(5), SubModel(5))){(router, initialState) => new MainComponent(router, initialState)}

        comp.component.childrenComponents.left.channel.onNext(MulNumber("0", 10))

        comp.channel.onNext(AddNumber("0", 5))

        assert(comp.component.childrenComponents.left.state != None)
        assert(comp.component.childrenComponents.left.state.get.value == 50)
      }
    }

//    it("should be able to handle lists") {
//      new BaseTest {
//        val comp = TestComponent[ListComponent](ListModel()){(router, initialState) => new ListComponent(router, initialState)}
//
//        comp.component.childrenComponents.subs.prependToList("ListComponent.subs")
//        //comp.component.childrenComponents.subs.childrenComponents(0)
//        comp.channel.onNext(AddItem)
//        comp.channel.onNext(AddItem)
//        println(list)
//      }
//    }

  }
}
