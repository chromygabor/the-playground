package com.chromy.reactiveui.core

/**
 * Created by cry on 2015.09.13..
 */
case class Service[A <: BaseComponent](name: String, context: Context[A#ModelType], val initialState: A#ModelType, service: A) {
  def update(action: Action, model: ComponentModel): A#ModelType = {
    context.chain.update(action, model.asInstanceOf[A#ModelType])
  }
}
