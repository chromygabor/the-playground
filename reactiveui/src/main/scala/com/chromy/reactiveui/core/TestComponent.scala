//package com.chromy.reactiveui.core
//
//import java.util.concurrent.Executor
//
//import rx.lang.scala.Subject
//import rx.lang.scala.subjects.BehaviorSubject
//
//import scala.concurrent.ExecutionContext
//import scala.util.{Failure, Success, Try}
//
///**
// * Created by cry on 2015.08.04..
// */
//
//trait ContextComponent[C <: BaseComponent] extends BaseComponent {
//  override type ModelType = C#ModelType
//
//  def context: Context[ModelType]
//
//  def component: C
//}
//
//object TestComponent {
//  def apply[C <: BaseComponent](iInitialState: C#ModelType)(f: (ContextMapper[C#ModelType], C#ModelType) => C): ContextComponent[C] = {
//    type M = C#ModelType
//    new ContextComponent[C] {
//      private lazy val name = s"DSP-ContextComponent"
//      println(s"[$name] created ")
//
//      val context = new Context[M] {
//        override val changes = BehaviorSubject[M]
//        override val chain: UpdateChain[M] = UpdateChain()
//        override val channel = Subject[Action]
//        override val initialState = iInitialState
//
//        val stream = channel.scan(initialState) { (oldState, action) =>
//          Try {
//            println(s"=================== [$name] action received  $action =================")
//            val newState = chain.update(action, oldState)
//            println(s"[$name] - An action received in the main loop: $action -- $oldState => $newState")
//            newState
//          } match {
//            case Success(newState) => newState
//            case Failure(error) =>
//              error.printStackTrace()
//              oldState
//          }
//        }
//
//        stream.drop(1) subscribe ({ newState =>
//          println(s"[$name] - A publishing a change: $newState")
//          changes.onNext(newState)
//        })
//
//        override def backgroundExecutor: ExecutionContext = ExecutionContext.fromExecutor(new Executor {
//          override def execute(command: Runnable): Unit = command.run()
//        })
//      }
//      val component = f(context.mapper, iInitialState)
//    }
//  }
//}
