package com.chromy.reactiveui.myjavafx

import javafx.scene.Scene
import javafx.stage.Stage

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.myjavafx.DialogService.{DialogManifestModel, SimpleDialog, Dialog}
import monocle.macros.GenLens

import rx.lang.scala.Subscriber
import com.chromy.reactiveui._

import scala.util.{Failure, Success}

/**
 * Created by chrogab on 2015.09.09..
 */

object DialogService {

  trait Dialog[A <: UiComponent] {
    def clazz: Class[A]
  }

  case class SimpleDialog[A <: UiComponent : Manifest](openerUid: Uid) extends Dialog[A] {
    def clazz = manifest[A].runtimeClass.asInstanceOf[Class[A]]
  }

  case class DialogManifestModel(openerUid: Uid, dialog: Dialog[_ <: UiComponent], dialogState: Option[_<: UiModel] = None,  uid: Uid = Uid()) extends core.Model[DialogManifest]

  case class Model(dialogs: List[DialogManifestModel] = List(), uid: Uid = Uid()) extends core.Model[DialogService]

}

class DialogManifest(val contextMapper: ContextMapper[DialogService.DialogManifestModel], val initialState: DialogService.DialogManifestModel) extends Component[DialogService.DialogManifestModel] {
  val dialogBuilder = initialState.dialog

  println(dialogBuilder.clazz)

  override protected def update(model: ModelType): Updater[ModelType] = Simple {
    case _ =>
      println("DialogManifestUpdate")
      model
  }
}


class DialogService(val contextMapper: ContextMapper[DialogService.Model], val initialState: DialogService.Model) extends Component[DialogService.Model] {
  def open[A <: UiComponent : Manifest](openerUid: Uid) = {
    val update: (Uid, DialogService.Model) => DialogService.Model = { (uid, model) =>
      model.copy(dialogs = DialogManifestModel(openerUid, SimpleDialog[A](uid)) :: model.dialogs)
    }
    channel.onNext(Defer(uid, update))
  }

  class Children {
    val dialogs = ListComponentOf[DialogManifest](context.map(GenLens[DialogService.Model](_.dialogs))) { (mapper, initialState) => new DialogManifest(mapper, initialState) }
  }

  val children = new Children
}

trait DialogServiceController extends JavaFXModule {
  type Component <: UiComponent
  type ModelType <: UiModel

  val dialogService = CounterApp.service[DialogService]

  //  lazy val dialogSubscriber = new Subscriber[DialogService.Dialog] {
  //    override def onNext(dialog: Dialog): Unit = {
  //      println(s"Here we need to open dialog: $dialog")
  ////      dialog match {
  ////        case e: SimpleDialog =>
  ////          JavaFXModule[CountersController](context.map(GenLens[CounterAppModel](_.counters)), initialState.counters) match {
  ////            case Success((parent, appController, appComponent)) =>
  ////              _component = appComponent
  ////              val stage = new Stage
  ////              stage.setScene(new Scene(parent))
  ////              stage.setTitle("CounterPair App")
  ////              //stage.setOnHidden() Send a close event to remove from the context and release the promise
  ////              stage.show()
  ////            case Failure(e) =>
  ////              e.printStackTrace()
  ////          }
  ////
  ////      }
  //    }
  //  }
  lazy val subscriber = new Subscriber[List[Operation[DialogManifest]]]() {

    override def onNext(value: List[Operation[DialogManifest]]): Unit = {
      value.filter {
        case e: AddItem[_] => true
        case _ => false
      }.foreach { case AddItem(dialog, _) =>
        dialog match {
          case e: SimpleDialog[_] => createSimpleDialog(e)
        }
      }

    }
  }

  private[this] def createSimpleDialog(manifest: DialogManifest): Unit = {


    //JavaFXModule[DialogManifestController](context.map(GenLens[CounterAppModel](_.counters)), initialState.counters)
  }


  abstract override def dispatcher(component: Component): Component = {
    dialogService.children.dialogs.subscribe(subscriber)
    super.dispatcher(component)
  }

}
