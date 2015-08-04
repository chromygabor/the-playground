package com.chromy.reactiveui.core

import rx.lang.scala.Subject
import rx.lang.scala.subjects.BehaviorSubject

import scala.util.{Failure, Success, Try}

/**
 * Created by cry on 2015.08.04..
 */

trait RouterComponent[C <: Component] extends Component {
  override type ModelType = C#ModelType

  def router: Router[ModelType]

  def component: C
}

object TestComponent {
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
