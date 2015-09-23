package com.chromy.reactiveui.core

import java.util.concurrent.Executor
import javafx.application.Platform
import javafx.fxml.FXMLLoader
import javafx.scene.Parent

import rx.lang.scala.{Scheduler => ScalaScheduler}
import rx.schedulers.Schedulers

import scala.util.Try

object JavaFXScheduler {

  lazy val executor = new Executor {
    override def execute(command: Runnable): Unit = Platform.runLater(command)
  }

  def apply() = {
    new ScalaScheduler {
      val asJavaScheduler = Schedulers.from(executor)
    }
  }
}


trait JavaFXController {
  type Controller
  type Model
  type Component

  def dispatcher(component: Component): Component = dispatch(component)

  protected def dispatch(component: Component): Component
}

abstract class GenericJavaFXController[A <: BaseComponent] extends JavaFXController {
  type Controller = this.type
  type Model = A#ModelType
  type Component = A
}

object JavaFXController {

  def componentType[A <: JavaFXController : Manifest]: reflect.runtime.universe.Type = {
    import reflect.runtime.{currentMirror => mirror}
    import scala.reflect.runtime.universe._

    val m = typeOf[A].member("Component": TypeName)
    val tpe = typeOf[A]
    m.asType.toTypeIn(tpe)
  }

  def apply[M <: JavaFXController : Manifest](component: M#Component): Try[(Parent, M, M#Component)] = {
    val ct = manifest[M]

    Try {
      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
      val r = ct.runtimeClass.getResource(s"$clazzName.fxml")

      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))

      val node: Parent = loader.load()
      val controller = loader.getController[M]
      controller.dispatch(component.asInstanceOf[controller.Component])
      (node, controller, component)
    }
  }

  def apply[M <: JavaFXController : Manifest](contextMapper: ContextMapper[M#Model]): Try[(Parent, M, M#Component)] = {
    import reflect.runtime.{currentMirror => mirror}

    val ct = manifest[M]

    scala.util.Try {
      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))

      val typeOfComponent = JavaFXController.componentType[M]

      val ctor = typeOfComponent.typeSymbol.asClass.primaryConstructor

      val classMirror = mirror.reflectClass(typeOfComponent.typeSymbol.asClass)
      val component = classMirror.reflectConstructor(ctor.asMethod).apply(contextMapper).asInstanceOf[M#Component]

      val node: Parent = loader.load()
      val controller = loader.getController[M]
      controller.dispatch(component.asInstanceOf[controller.Component])
      (node, controller, component)
    }
  }

}
