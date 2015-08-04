package com.chromy.reactiveui

import java.util.UUID

import com.chromy.reactiveui.myjavafx.CounterApp.Nop
import com.chromy.reactiveui.myjavafx._
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observable, Observer, Subject}

import scala.util.{Failure, Success, Try}

/**
 * Created by cry on 2015.07.11..
 */
trait Component {
  type ModelType <: com.chromy.reactiveui.Model[_ <: Component]

  def router: Router[ModelType]
}

trait Model[C <: Component] {
  def uid: String
}

trait BaseComponent[M <: Model[_ <: Component]] extends Component {
  type ModelType = M

  protected def routerMapper: RouterMapper[ModelType]

  protected def initialState: ModelType

  def update: (Action, ModelType, Observer[Action]) => ModelType

  /** Private parts **/
  case class ActualState(action: Action, state: ModelType) extends ActionWrapper

  case class Step(action: Action, state: ModelType) extends Action

  private val _channel = Subject[Action]
  private lazy val _changes = BehaviorSubject[ModelType]

  private lazy val name = s"DSP-${this.getClass.getSimpleName}(${initialState.uid})"
  println(s"[$name] created with $initialState")

  def subscriber: (Action, ModelType, ModelType) => ModelType = { (action, _, prevState) =>
    println(s"[$name] a new state was requested for $prevState and $action")

    action match {
      case ActualState(_, actualState) if (actualState.uid == initialState.uid) =>
        println(s"[$name] action is ActualState so we just unwrap it => $actualState")
        actualState
      case e: ActionWrapper =>
        val newState = update(e.action, prevState, _channel)
        println(s"[$name] action is ActionWrapper so unwrap it and call update => $newState")
        _channel.onNext(Step(e.action, newState))
        newState
      case e: Action =>
        val newState = update(e, prevState, _channel)
        println(s"[$name] action is a simple action so we call update => $newState")
        _channel.onNext(Step(e, newState))
        newState
      case _ =>
        println(s"[$name] something went wrong, we return the prevState: $prevState")
        prevState
    }
  }

  val router = routerMapper(subscriber)

  router.changes.distinctUntilChanged.subscribe(
  { change =>
    println(s"[$name] new change from parent: $change")
    _changes.onNext(change)
  }, { error => _changes.onError(error) }, { () => _changes.onCompleted() }
  )

  private val stream = _channel.scan((initialState, initialState, Nop.asInstanceOf[Action])) { case ((beforePrevState, prevState, prevAction), action) =>
    Try[ModelType] {
      action match {
        case Step(action, newState) =>
          newState
        case StateChange(actionToWrap, actualState: ModelType) =>
          println(s"=================== [$name] action received  $action =================")
          println(s"[$name] action is ActualState $actualState")
          actualState
        case action: ActionWrapper =>
          println(s"[$name] action is already a WrapperAction so return the previous state")
          prevState
        case action =>
          println(s"=================== [$name] action received  $action =================")
          val newState = update(action, prevState, _channel)
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
            _changes.onNext(actualState)
          case action@StateChange(a, s: ModelType) =>
            val wrap = ActualState(a, s)
            println(s"[$name] action is StateChange sending to channel as a $wrap")
            router.channel.onNext(wrap)
          case action: ActionWrapper =>
            println(s"[$name] action is already a WrapperAction so just send up to channel: ${action}")
            router.channel.onNext(action)
          case action =>
            println(s"[$name] action is a simple action wrapping to ActualState and sending to channel: ${ActualState(action, actualState)}")
            router.channel.onNext(ActualState(action, actualState))
        }
    }
  })
  _changes.onNext(initialState)
}

sealed trait Operation[M] extends Model[ListComponentOf[_]] {
  val uid = UUID.randomUUID().toString
}

case class DeleteItem[M](item: M, index: Int) extends Operation[M]

case class AddItem[M](item: M, index: Int) extends Operation[M]


/**
 * ListComponentOf
 * @tparam M
 */
trait ListComponentOf[M <: Model[_ <: Component]] extends Component {

  case class ListCompononentOfModel(routersToAdd: Map[String, (Int, ComponentType)], routersToRemove: Map[String, (Int, ComponentType)], uid: String = Uid.nextUid().toString) extends Model[ListComponentOf[M]]

  type ComponentType = Component {type ModelType = M}
  type ModelType = Operation[ComponentType]

  protected def routerMapper: RouterMapper[List[M]]

  protected def create: Router[M] => ComponentType

  private lazy val name = s"DSP-ListComponentOf}"
  println(s"[$name] created with")

  val stream = Subject[ModelType]

  private var _components = Map[String, ComponentType]()

  def childrenComponents = _components

  def childrenComponents_=(newComponents: Map[String, ComponentType]) = _components = newComponents


  val myRouter = {
    println("myRouter created")
    routerMapper { (action, original, state) =>
      println(s"[$name] a new state was requested for $original and $action")

      val (toDelete, toInsert) = ListDiff.diff(original, state) {
        _.uid
      }

      toDelete.foldLeft((0, List[(M, Int)]())) { case ((removedItems, list), (item, index)) =>
        val newItem = item -> (index - removedItems)
        (removedItems + 1, newItem :: list)
      }._2.foreach { case (item, index) =>
        val component = childrenComponents(item.uid)
        childrenComponents = childrenComponents - item.uid
        stream.onNext(DeleteItem(component, index))
      }

      toInsert.foreach { case (item, index) =>
        val component = createComponent(item)
        childrenComponents = childrenComponents + (item.uid -> component)
        stream.onNext(AddItem(component, index))
      }

      state.map { in =>
        childrenComponents.get(in.uid) match {
          case Some(component) => component.router.chain.update(action, in)
          case _ => in
        }
      }
    }
  }

  private val myChanges = myRouter.changes.map(_.map { in => in.uid -> in }.toMap)

  def createComponent(item: M): ComponentType = {
    create(new Router[M] {
      override val changes: Observable[M] = myChanges.map { change => change(item.uid) }
      override val chain: UpdateChain[M] = UpdateChain[M]
      override val channel: Observer[Action] = myRouter.channel
    })
  }

  val router = {
    println("router created")
    new Router[ModelType] {
      override def changes: Observable[ModelType] = stream.sample(myRouter.changes)

      override def chain: UpdateChain[ModelType] = {
        throw new IllegalStateException("This shouldn't be called because never registered")
      }

      override def channel: Observer[Action] = myRouter.channel
    }
  }
}


object ListComponentOf {
  def apply[M <: Model[_ <: Component]](iRouterMapper: RouterMapper[List[M]])(iCreate: (Router[M] => Component {type ModelType = M})): ListComponentOf[M] = {
    val listComponentOf = new ListComponentOf[M] {
      override def routerMapper = iRouterMapper

      override def create: (Router[M]) => Component {type ModelType = M} = iCreate

    }
    listComponentOf
  }
}

object TestComponent {

  trait RouterComponent[C <: Component] extends Component {
    override type ModelType = C#ModelType

    def router: Router[ModelType]

    def component: C
  }

  def apply[C <: Component](initialState: C#ModelType)(f: (RouterMapper[C#ModelType], C#ModelType) => C): RouterComponent[C] = {
    type M = C#ModelType
    new RouterComponent[C] {
      private lazy val name = s"DSP-RouterComponent"
      println(s"[$name] created ")

      val router = new Router[M] {
        override val changes = BehaviorSubject[M]
        override val chain: UpdateChain[M] = UpdateChain()
        override val channel = Subject[Action]

        val stream = channel.scan(initialState) { (oldState, action) =>
          Try {
            println(s"=================== [$name] action received  $action =================")
            val newState = chain.update(action, oldState)
            println(s"[$name] - An action received in the main loop: $action -- $oldState => $newState")
            newState
          } match {
            case Success(newState) => newState
            case Failure(error) =>
              error.printStackTrace()
              oldState
          }
        }

        stream.drop(1) subscribe ({ newState =>
          println(s"[$name] - A publishing a change: $newState")
          changes.onNext(newState)
        })

      }
      val component = f(router.mapper, initialState)
    }
  }
}