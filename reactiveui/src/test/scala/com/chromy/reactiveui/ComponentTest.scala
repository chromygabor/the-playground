package com.chromy.reactiveui

import org.scalatest.FunSpecLike

/**
* Created by cry on 2015.07.05..
*/
class ComponentTest extends FunSpecLike {
  describe("Component") {
    it("should do") {
      new BaseTest {
        val comp = TestComponent[MainComponent](MainModel()){router => new MainComponent(router)}
        comp.channel.onNext(Add(10))
        assert(comp.state.left.value == 10)
        assert(comp.state.right.value == 10)
      }
    }
  }
}
