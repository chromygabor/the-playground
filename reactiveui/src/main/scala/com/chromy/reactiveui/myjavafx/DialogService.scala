package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import com.chromy.reactiveui._
import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.Executable
import monocle.macros.GenLens
import rx.lang.scala.{Observable, Observer, Subscriber}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

/**
 * Created by chrogab on 2015.09.09..
 */

object DialogService {
  case class Model(dialogs: List[DialogManifestModel[_ <: UiModel]] = List(), uid: Uid = Uid()) extends core.Model[DialogService]
}

case class DialogManifestModel[A <: UiModel](openerUid: Uid, dialogState: A, clazz: Manifest[_], uid: Uid = Uid()) extends core.Model[DialogManifest] {
}

class DialogManifest(val contextMapper: ContextMapper[DialogManifestModel[_ <: UiModel]], val iInitialState: DialogManifestModel[_ <: UiModel]) extends Component[DialogManifestModel[_ <: UiModel]] {

  val ctx = new Context[UiModel] {
    override val changes = context.changes.map(_.dialogState)

    override val chain: UpdateChain[UiModel] = UpdateChain[UiModel]

    override val initialState: UiModel = iInitialState.dialogState

    override val channel: Observer[Action] = context.channel

    override val backgroundExecutor: ExecutionContext = context.backgroundExecutor
  }

  val component = initialState.clazz.runtimeClass.getConstructor(classOf[ContextMapper[_]]).newInstance(ctx.mapper).asInstanceOf[UiComponent]

  override protected def update(model: ModelType): Updater[ModelType] = Simple {
    case action =>
      val newState = ctx.chain.update(action, model.dialogState)
      model.copy(dialogState = newState.asInstanceOf[UiModel])
  }
}


class DialogService(val contextMapper: ContextMapper[DialogService.Model]) extends Component[DialogService.Model] {
  def open[A <: UiComponent : Manifest](openerUid: Uid, input: A#ModelType) = {
    val update: (Uid, DialogService.Model) => DialogService.Model = { (uid, model) =>
      model.copy(dialogs = DialogManifestModel(uid, input, manifest[A]) :: model.dialogs)
    }
    channel.onNext(Defer(uid, update))
  }

  def createChild(contextMapper: ContextMapper[DialogManifest#ModelType], initialState: DialogManifest#ModelType): DialogManifest = {
    new DialogManifest(contextMapper, initialState)
  }

  class Children {
    val dialogs = ListComponentOf[DialogManifest](context.map(GenLens[DialogService.Model](_.dialogs))) { (mapper, initialState) => createChild(mapper, initialState) }
  }

  val children = new Children
}

trait DialogServiceController extends JavaFXController {
  type Component <: UiComponent
  type ModelType <: UiModel

  val dialogService = CounterApp.service[DialogService]

  private lazy val dialogSubscriber: List[Operation[DialogManifest]] => Executable = { value =>
    Executable {
      value.filter {
        case e: AddItem[_] => true
        case _ => false
      }.foreach { case AddItem(dialog, _) =>
        Try {
          val dialogClazz = dialog.iInitialState.clazz.runtimeClass.asSubclass(classOf[UiComponent])
          val controllerClass = Services.dialogs.get(dialogClazz)
          controllerClass.foreach { controllerClz =>
            val clazzName = controllerClz.getSimpleName
            val loader = new FXMLLoader(dialog.iInitialState.clazz.runtimeClass.getResource(s"$clazzName.fxml"))
            val node: Parent = loader.load()
            val controller = loader.getController[JavaFXController]
            controller.dispatcher(dialog.component.asInstanceOf[controller.Component])

            val stage = new Stage
            stage.setScene(new Scene(node))
            stage.setTitle("CounterPair App")
            stage.show()
          }
        } match {
          case Success(_) =>
          case Failure(e) => e.printStackTrace()
        }
      }
    }
  }

  abstract override def dispatcher(component: Component): Component = {
    dialogService.children.dialogs.subscribe(dialogSubscriber)
    super.dispatcher(component)
  }

}
