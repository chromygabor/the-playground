package com.chromy.frpui.fw.javafx

import javafx.fxml.FXMLLoader
import javafx.scene.Parent

import com.chromy.frpui.fw.core.{Event, SideEffectChain}

import scala.util.Try

/**
 * Created by cry on 2015.11.01..
 */
object JavaFX {
  def apply[M <: BaseController : Manifest](channel: Event => Unit, render: SideEffectChain[M#C], initialState: M#C): Try[(Parent, M)] = {
    val ct = manifest[M]

    Try {
      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName

      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))

      val node: Parent = loader.load()
      val controller = loader.getController[M]

      val sideEffect = controller.init(channel, render.asInstanceOf[SideEffectChain[controller.C]], initialState.asInstanceOf[controller.C])

      sideEffect.run()
      //      sideEffect.errors.foreach(_.printStackTrace())

      (node, controller)
    }

  }

  //  def apply[M <: JavaFXController : Manifest](component: M#Component): Try[(Parent, M, M#Component)] = {
  //    val ct = manifest[M]
  //
  //    Try {
  //      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
  //
  //      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))
  //
  //      val node: Parent = loader.load()
  //      val controller = loader.getController[M]
  //      controller.dispatch(component.asInstanceOf[controller.Component])
  //      //component.context.changes.update(component.context.initialState).run()
  //      (node, controller, component)
  //    }
  //  }
  //
  //  def apply[M <: JavaFXController : Manifest](contextMapper: ContextMapper[M#Model]): Try[(Parent, M, M#Component)] = {
  //    import reflect.runtime.{currentMirror => mirror}
  //
  //    val ct = manifest[M]
  //
  //    scala.util.Try {
  //      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
  //      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))
  //
  //      val typeOfComponent = JavaFXController.componentType[M]
  //
  //      val ctor = typeOfComponent.typeSymbol.asClass.primaryConstructor
  //
  //      val classMirror = mirror.reflectClass(typeOfComponent.typeSymbol.asClass)
  //      val component = classMirror.reflectConstructor(ctor.asMethod).apply(contextMapper).asInstanceOf[M#Component]
  //
  //      val node: Parent = loader.load()
  //      val controller = loader.getController[M]
  //      controller.dispatch(component.asInstanceOf[controller.Component])
  //      //component.context.changes.update(component.context.initialState).run()
  //      (node, controller, component)
  //    }
  //  }

}
