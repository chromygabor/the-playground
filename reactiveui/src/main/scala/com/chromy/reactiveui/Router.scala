package com.chromy.reactiveui

import com.chromy.reactiveui.myjavafx._
import monocle.Lens
import rx.lang.scala.{Observable, Observer}

/**
 * Created by cry on 2015.07.05..
 */

trait Router[T] {
  val changes: Observable[T]
  val channel: Observer[Action]
  val chain: UpdateChain[T]

  def map[B](lens: Lens[T, B]): Router[B] = {
    val parent = this
    new Router[B] {
      val changes = parent.changes.map { in => lens.get(in) }
      val channel = parent.channel
      val chain = parent.chain.fromLens(lens)
    }
  }
}


//trait SmartComponent[C <: RouterModel]  {
//  type Model = C#Model
//
//  val parentRouter: Router[C]
//  val initialState: Model
//
//  def update: (Action, Model,  Observer[Action]) => Model
//
//
//  val changes = parentRouter.changes
//  val actions = Subject[Action]
//
//  private lazy val name =  s"DSP-${this.getClass.getSimpleName}(${initialState.uid})"
//  println(s"[$name] created ")
//
//  case class ActualState(action: Action, state: Model) extends ActionWrapper
//  case class Step(action:Action, state: Model) extends Action
//
//  private lazy val renderer = Subject[Model]
//
//  private val stream = actions.scan((initialState, initialState, Nop.asInstanceOf[Action])) { case ((beforePrevState, prevState, prevAction), action) =>
//    Try[Model] {
//      action match {
//        case Step(action, newState) =>
//          newState
//        case StateChange(actionToWrap, actualState: Model) =>
//          println(s"=================== [$name] action received  $action =================")
//          println(s"[$name] action is ActualState $actualState")
//          actualState
//        case action: ActionWrapper =>
//          println(s"[$name] action is already a WrapperAction so return the previous state")
//          prevState
//        case action =>
//          println(s"=================== [$name] action received  $action =================")
//          val newState = update(action, prevState, actions)
//          println(s"[$name] action triggered a new state: $prevState => $newState")
//          newState
//      }
//    } match {
//      case Success(newState) => (prevState, newState, action)
//      case Failure(error) =>
//        error.printStackTrace()
//        (beforePrevState, prevState, prevAction)
//    }
//  }
//
//  stream.drop(1).distinctUntilChanged.subscribe({ input =>
//    input match {
//      case (prevState, actualState, action) =>
//        action match {
//          case Step(_, _) =>
//            println(s"[$name] action is a step so nothing to do: $action")
//          case e: LocalAction =>
//            println(s"[$name] action is LocalAction triggering render: $actualState")
//            renderer.onNext(actualState)
//          case action@StateChange(a, s: Model) =>
//            val wrap = ActualState(a,s)
//            println(s"[$name] action is StateChange sending to channel as a $wrap")
//            parentRouter.channel.onNext(wrap)
//          case action: ActionWrapper =>
//            println(s"[$name] action is already a WrapperAction so just send up to channel: ${action}")
//            parentRouter.channel.onNext(action)
//          case action =>
//            println(s"[$name] action is a simple action wrapping to ActualState and sending to channel: ${ActualState(action, actualState)}")
//            parentRouter.channel.onNext(ActualState(action, actualState))
//        }
//    }
//  })
//
//  parentRouter.chain.subscribe { (action, prevState) =>
////    println(s"[$name] a new state was requested for $prevState and $action")
////
////    action match {
////      case ActualState(_, actualState: Model) if(actualState.uid == initialState.uid) =>
////        println(s"[$name] action is ActualState so we just unwrap it => $actualState")
////        actualState
////      case e: ActionWrapper =>
////        val newState = update(e.action, prevState, actions)
////        println(s"[$name] action is ActionWrapper so unwrap it and call update => $newState")
////        actions.onNext(Step(e.action, newState))
////        newState
////      case e: Action =>
////        val newState = update(e, prevState, actions)
////        println(s"[$name] action is a simple action so we call update => $newState")
////        newState
////      case _ =>
////        println(s"[$name] something went wrong, we return the prevState: $prevState")
////        prevState
////    }
//
//    prevState
//  }
//
////
////
////  def init() = renderer.onNext(initialState)
//
//}
