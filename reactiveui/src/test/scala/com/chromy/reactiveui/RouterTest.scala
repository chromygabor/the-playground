package com.chromy.reactiveui

import org.scalatest.FunSpecLike

/**
* Created by cry on 2015.07.05..
*/
class RouterTest extends FunSpecLike {

  trait Component {
    type Model <: { def uid: String }
    def update(model: Model) : Model
  }

  trait Model[C <: Component] {
    def uid: String
  }

  trait BaseComponent[M <: Model[_ <: Component]] extends Component {
    type Model = M
    def update(model: Model) : Model
  }

  case class MainModel(left: SubModel = SubModel(), right: SubModel = SubModel(), uid: String = "") extends Model[MainComponent]
  case class SubModel(value: Int = 0, uid: String = "") extends Model[SubComponent]

  class MainComponent(param: Int) extends BaseComponent[MainModel] {
    override def update(model: MainModel): MainModel = ???
  }
  class SubComponent(param: Int) extends BaseComponent[SubModel]  {
    override def update(model: SubModel): SubModel = ???
  }



  describe("Router") {
    it("should do") {
//      val router = new Router[MainComponent] {
//
//        override val changes = Subject[MainModel]
//        override val chain: UpdateChain[MainModel] = UpdateChain()
//        override val channel = Subject[Action]
//
//        val initModel = MainModel()
//        val stream = channel.observeOn(ComputationScheduler()).scan(initModel) { (oldState, action) =>
//          Try {
//            val newState = chain.update(action, oldState)
//            println(s"[DSP-Main(0)] - An action received in the main loop: $action -- $oldState => $newState")
//            newState
//          } match {
//            case Success(newState) => newState
//            case Failure(error) =>
//              error.printStackTrace()
//              oldState
//          }
//        }
//
//        //actions.subscribe({ in => println(s"[DSP-Main(0)] - An action received in the main loop: $in") })
//        //      changes.subscribe({ in => println(s"[DSP-MAIN] - A change is published from main loop: $in\n======================") })
//        stream.subscribe({ newState =>
//          println(s"[DSP-Main(0)] - A publishing a change: $newState")
//          changes.onNext(newState)
//        })
//      }
//
//      val comp = new MainComponent(router, MainModel())
//
//      comp.actions.onNext(Add(10))

    }
  }
}
