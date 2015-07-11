package com.chromy.reactiveui

import com.chromy.reactiveui.myjavafx.CounterApp.Nop
import com.chromy.reactiveui.myjavafx._
import org.scalatest.FunSpecLike
import rx.lang.scala.schedulers.ComputationScheduler
import rx.lang.scala.{Observable, Observer, Subject}

import scala.util.{Failure, Success, Try}

/**
 * Created by cry on 2015.07.05..
 */
class RouterTest extends FunSpecLike {

  object MainComponent {
    def update: (Action, MainModel, Observer[Action]) => MainModel = { (action, model, channel) =>
      model
    }

  }

  class MainComponent(parentRouter: Router[MainComponent], initialState: MainModel) {
    type Model = MainModel

    val changes = parentRouter.changes
    val actions = Subject[Action]

    private lazy val name =  s"DSP-${this.getClass.getSimpleName}(${initialState.uid})"
    println(s"[$name] created ")

    case class ActualState(action: Action, state: Model) extends ActionWrapper
    case class Step(action:Action, state: Model) extends Action

    private lazy val renderer = Subject[Model]

    private val stream = actions.scan((initialState, initialState, Nop.asInstanceOf[Action])) { case ((beforePrevState, prevState, prevAction), action) =>
      Try[Model] {
        action match {
          case Step(action, newState) =>
            newState
          case StateChange(actionToWrap, actualState: Model) =>
            println(s"=================== [$name] action received  $action =================")
            println(s"[$name] action is ActualState $actualState")
            actualState
          case action: ActionWrapper =>
            println(s"[$name] action is already a WrapperAction so return the previous state")
            prevState
          case action =>
            println(s"=================== [$name] action received  $action =================")
            val newState = update(action, prevState, actions)
            println(s"[$name] action triggered a new state: $prevState => $newState")
            newState
        }
      } match {
        case Success(newState) => (prevState, newState, action)
        case Failure(error) =>
          error.printStackTrace()
          (beforePrevState, prevState, prevAction)
      }
    }

    stream.drop(1).distinctUntilChanged.subscribe({ input =>
      input match {
        case (prevState, actualState, action) =>
          action match {
            case Step(_, _) =>
              println(s"[$name] action is a step so nothing to do: $action")
            case e: LocalAction =>
              println(s"[$name] action is LocalAction triggering render: $actualState")
              renderer.onNext(actualState)
            case action@StateChange(a, s: Model) =>
              val wrap = ActualState(a,s)
              println(s"[$name] action is StateChange sending to channel as a $wrap")
              parentRouter.channel.onNext(wrap)
            case action: ActionWrapper =>
              println(s"[$name] action is already a WrapperAction so just send up to channel: ${action}")
              parentRouter.channel.onNext(action)
            case action =>
              println(s"[$name] action is a simple action wrapping to ActualState and sending to channel: ${ActualState(action, actualState)}")
              parentRouter.channel.onNext(ActualState(action, actualState))
          }
      }
    })

    parentRouter.chain.subscribe { (action, prevState) =>
      println(s"[$name] a new state was requested for $prevState and $action")

      action match {
        case ActualState(_, actualState: Model) if(actualState.uid == initialState.uid) =>
          println(s"[$name] action is ActualState so we just unwrap it => $actualState")
          actualState
        case e: ActionWrapper =>
          val newState = update(e.action, prevState, actions)
          println(s"[$name] action is ActionWrapper so unwrap it and call update => $newState")
          actions.onNext(Step(e.action, newState))
          newState
        case e: Action =>
          val newState = update(e, prevState, actions)
          println(s"[$name] action is a simple action so we call update => $newState")
          newState
        case _ =>
          println(s"[$name] something went wrong, we return the prevState: $prevState")
          prevState
      }
    }

    def update: (Action, Model,  Observer[Action]) => Model = MainComponent.update

    def subscribe()

    def init() = renderer.onNext(initialState)

    lazy val router = new Router[MainComponent] {
      override val changes = MainComponent.this.changes
      override val chain = MainComponent.this.parentRouter.chain
      override val channel = MainComponent.this.actions
    }
  }


  describe("Router") {
    it("should do") {
      val router = new Router[MainComponent] {

        override val changes = Subject[MainModel]
        override val chain: UpdateChain[MainModel] = UpdateChain()
        override val channel = Subject[Action]

        val initModel = MainModel()
        val stream = channel.observeOn(ComputationScheduler()).scan(initModel) { (oldState, action) =>
          Try {
            val newState = chain.update(action, oldState)
            println(s"[DSP-Main(0)] - An action received in the main loop: $action -- $oldState => $newState")
            newState
          } match {
            case Success(newState) => newState
            case Failure(error) =>
              error.printStackTrace()
              oldState
          }
        }

        //actions.subscribe({ in => println(s"[DSP-Main(0)] - An action received in the main loop: $in") })
        //      changes.subscribe({ in => println(s"[DSP-MAIN] - A change is published from main loop: $in\n======================") })
        stream.subscribe({ newState =>
          println(s"[DSP-Main(0)] - A publishing a change: $newState")
          changes.onNext(newState)
        })
      }

      val comp = new MainComponent(router, MainModel())

      comp.actions.onNext(Add(10))

    }
  }
}
