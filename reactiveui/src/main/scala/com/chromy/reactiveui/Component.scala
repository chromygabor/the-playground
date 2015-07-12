package com.chromy.reactiveui

import com.chromy.reactiveui.myjavafx.CounterApp.Nop
import com.chromy.reactiveui.myjavafx.{Action, ActionWrapper, LocalAction, StateChange}
import rx.lang.scala.{Observer, Subject}

import scala.util.{Failure, Success, Try}

/**
 * Created by cry on 2015.07.11..
 */
trait Component {
  type Model <: {def uid: String}
}

trait Model[C <: Component] {
  def uid: String
}

trait BaseComponent[M <: Model[_ <: Component]] extends Component {
  type Model = M

  val parentRouter: Router[Model]
  val initialState: Model

  def update: (Action, Model, Observer[Action]) => Model

  val changes = parentRouter.changes
  val actions = Subject[Action]

  private lazy val name = s"DSP-${this.getClass.getSimpleName}(${initialState.uid})"
  println(s"[$name] created ")

  case class ActualState(action: Action, state: Model) extends ActionWrapper

  case class Step(action: Action, state: Model) extends Action

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
            val wrap = ActualState(a, s)
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
      case ActualState(_, actualState: Model) if (actualState.uid == initialState.uid) =>
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
}

class ListComponentOf[M <: Model[_ <: Component]](parentRouter: Router[_<: Seq[M]]) (create: (Router[M] => Component { type Model = M})) extends Component {

}

object TestComponent {

  trait RouterComponent[M <: {def uid : String}] extends Component {
    override type Model = M
    val router: Router[M]
    protected var _state: M = _

    def state = _state

    lazy val channel = router.channel
  }

  def apply[C <: Component](initialModel:C#Model)(f: Router[C#Model] => C): RouterComponent[C#Model] = {
    type M = C#Model
    new RouterComponent[M] {
      val router = new Router[M] {
        override val changes = Subject[M]
        override val chain: UpdateChain[M] = UpdateChain()
        override val channel = Subject[Action]

        val stream = channel.scan(initialModel) { (oldState, action) =>
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

        stream.drop(1) subscribe ({ newState =>
          _state = newState
          println(s"[DSP-Main(0)] - A publishing a change: $newState")
          changes.onNext(newState)
        })

      }
      val component = f(router)
    }
  }
}
