package com.chromy.frpui.javafxapp

import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import com.chromy.frpui.core.{BaseModel, FrpApp, SideEffect, SideEffectChain}

import scala.util.{Failure, Success, Try}

/**
 * Created by chrogab on 2015.10.30..
 */

trait BaseController {
  type C <: BaseModel
  
  def subscriber: C => SideEffect
  
  def init(render: SideEffectChain[C], initialState: C ) = {
    render.subscribe(subscriber)
  }
}
trait Controller[COMP <: BaseModel] extends BaseController {
  type C = COMP
}

object JavaFXController {
  def apply[M <: BaseController : Manifest](render: SideEffectChain[M#C], initialState: M#C): Try[(Parent, M)] = {
    import reflect.runtime.{currentMirror => mirror}

    val ct = manifest[M]

    Try {
      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName

      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))

      val node: Parent = loader.load()
      val controller = loader.getController[M]

      controller.init(render.asInstanceOf[SideEffectChain[controller.C]], initialState.asInstanceOf[controller.C])
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

object CountersApp extends App {

  new JFXPanel()

  val app = new FrpApp[Counters](Counters(), Map.empty)
  
  Platform.runLater(new Runnable() {
    override def run(): Unit = {
      JavaFXController[CountersController](app.render, app.initialState) match {
        case Success((parent, _)) =>
          val stage = new Stage
          stage.setScene(new Scene(parent))
          stage.setTitle("CounterPair App")
          stage.show()
        case Failure(e) =>
          e.printStackTrace()
      }

    }
  })
}
