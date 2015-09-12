package com.chromy.reactiveui.myjavafx

import com.chromy.reactiveui.core.{ContextMapper, Uid, Model, Component}

/**
 * Created by chrogab on 2015.09.09..
 */

case class DialogServiceModel(uid: Uid = Uid()) extends Model[DialogService]

class DialogService(val contextMapper: ContextMapper[DialogServiceModel], val initialState: DialogServiceModel) extends Component[DialogServiceModel] {

}

class DialogServiceController extends GenericJavaFXModule[DialogService] {
  override def dispatch(component: Component): Component = ???
}
