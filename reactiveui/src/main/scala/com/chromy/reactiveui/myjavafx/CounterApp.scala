package com.chromy.reactiveui.myjavafx

import java.util.concurrent.{Executor, Executors}
import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.myjavafx.CounterNavigatorDialog
import monocle.macros.GenLens
import rx.lang.scala.schedulers.IOScheduler
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observable, Observer, Scheduler => ScalaScheduler, Subject}
import rx.schedulers.Schedulers
import scala.collection.mutable.{HashMap => MMap, WeakHashMap => WMap}


import scala.concurrent.ExecutionContext
import scala.util.Try


/**
 * Created by chrogab on 2015.06.04..
 */

trait ServiceBuilder[A <: BaseComponent] {
  def apply(context: Context[A#ModelType]): (A#ModelType, A)
}

object Singleton {
  def apply[A <: ComponentModel](initialState: A)(create: (ContextMapper[A], A) => A#ComponentType): ServiceBuilder[A#ComponentType] = new ServiceBuilder[A#ComponentType] {
    private[this] var _instance = Option.empty[A#ComponentType]
    type ModelType = A
    type ComponentType = A#ComponentType

    override def apply(context: Context[A#ComponentType#ModelType]): (A#ComponentType#ModelType, A#ComponentType) = {
      (initialState.asInstanceOf[A#ComponentType#ModelType], _instance.getOrElse {
        val component = create(context.mapper.asInstanceOf[ContextMapper[A]], initialState)
        _instance = Some(component)
        _instance.get
      })
    }
  }
}

object Services {

  lazy val app = (context: ContextMapper[CountersModel], initialState: CountersModel) => JavaFXModule[CountersController](context, initialState)

  lazy val dialogs = Map(
    classOf[CounterNavigatorDialog] -> classOf[CounterNavigatorDialog]
  )

  lazy val services = Map(
    "app" -> Singleton(CountersModel()) { (contextMapper, initialState) => new Counters(contextMapper, initialState) },
    "com.chromy.reactiveui.myjavafx.CounterStore" -> Singleton(CounterStore.Model()) { (contextMapper, initialState) => new CounterStore(contextMapper, initialState) },
    "com.chromy.reactiveui.myjavafx.DialogService" -> Singleton(DialogService.Model()) { (contextMapper, initialState) => new DialogService(contextMapper, initialState) }
  )
}

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


trait JavaFXModule {
  type Controller
  type Model
  type Component

  def dispatcher(component: Component): Component = dispatch(component)

  protected def dispatch(component: Component): Component
}

case class JavaFxModuleWrapper[A <: JavaFXModule](parent: Parent, controller: A, component: A#Component)

object JavaFXModule {

  def componentType[A <: JavaFXModule : Manifest]: reflect.runtime.universe.Type = {
    import reflect.runtime.{currentMirror => mirror}
    import scala.reflect.runtime.universe._

    val m = typeOf[A].member("Component": TypeName)
    val tpe = typeOf[A]
    m.asType.toTypeIn(tpe)
  }

  def apply[M <: JavaFXModule : Manifest](component: M#Component): Try[(Parent, M, M#Component)] = {
    val ct = manifest[M]

    Try {
      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))

      val node: Parent = loader.load()
      val controller = loader.getController[M]
      controller.dispatch(component.asInstanceOf[controller.Component])
      (node, controller, component)
    }
  }

  def apply[M <: JavaFXModule : Manifest](contextMapper: ContextMapper[M#Model], initialState: M#Model): Try[(Parent, M, M#Component)] = {
    import reflect.runtime.{currentMirror => mirror}

    val ct = manifest[M]

    scala.util.Try {
      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName
      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))

      val typeOfComponent = JavaFXModule.componentType[M]

      val ctor = typeOfComponent.typeSymbol.asClass.primaryConstructor

      val classMirror = mirror.reflectClass(typeOfComponent.typeSymbol.asClass)
      val component = classMirror.reflectConstructor(ctor.asMethod).apply(contextMapper, initialState).asInstanceOf[M#Component]

      val node: Parent = loader.load()
      val controller = loader.getController[M]
      controller.dispatch(component.asInstanceOf[controller.Component])
      (node, controller, component)
    }
  }

}

abstract class GenericJavaFXModule[A <: BaseComponent] extends JavaFXModule {
  type Controller = this.type
  type Model = A#ModelType
  type Component = A
}

case class CounterAppModel(counters: CountersModel = CountersModel(), services: Map[String, _ <: ComponentModel] = Map())

object CounterApp extends App {

  import scala.util.{Failure, Success, Try}

  type C = Counters
  type M = CounterAppModel

  val initialState = CounterAppModel()

  val fxPanel = new JFXPanel()

  var _component: C = _

  val context: Context[M] = new Context[M] {

    lazy val name = s"DSP-MainComponent"
    println(s"[$name] created ")

    override val changes = BehaviorSubject[M]
    override val chain: UpdateChain[M] = UpdateChain()
    override val channel = Subject[Action]

    val chainSch = IOScheduler()
    val changesSch = JavaFXScheduler()
    override val backgroundExecutor: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

    println(s"App initialstate: $initialState")

    val stream = channel.observeOn(chainSch).scan(initialState) { (oldState, action) =>
      Try {
        println(s"=================== [$name] action received  $action ================= on ${Thread.currentThread.getName}")
        println(s"[$name] - An action received in the main loop: $action, updating state $oldState...")
        val newState = chain.update(action, oldState)
        println(s"[$name] - result is: $newState")
        newState
      } match {
        case Success(newState) => newState
        case Failure(error) =>
          error.printStackTrace()
          oldState
      }
    }

    stream.drop(1).observeOn(changesSch).subscribe({ newState =>
      println(s"[$name] - A publishing a change: $newState, on: ${Thread.currentThread().getName}")
      changes.onNext(newState)
    })

  }

  Platform.runLater(new Runnable() {
    override def run(): Unit = {
      JavaFXModule[CountersController](context.map(GenLens[CounterAppModel](_.counters)), initialState.counters) match {
        case Success((parent, appController, appComponent)) =>
          _component = appComponent
          val stage = new Stage
          stage.setScene(new Scene(parent))
          stage.setTitle("CounterPair App")
          stage.show()
        case Failure(e) =>
          e.printStackTrace()
      }

    }
  })

  private[this] val serviceSubscriber: (Action, Map[String, ComponentModel], Map[String, ComponentModel]) => Map[String, ComponentModel] = { (action, _, model) =>

    // Map wich contains all the updated models of which are both in the Repository and the model
    // We eliminated all models which are missing in Repository
    val nm: List[(String, ComponentModel)] = model.map { case (serviceName, serviceState) =>
      val oService = services.find {
        case (_, Service(`serviceName`, _, _, _)) => true
        case _ => false
      }
      for {
        (_, service) <- oService
      } yield {
        val newState = service.update(action, serviceState)
        (serviceName, newState.asInstanceOf[ComponentModel])
      }
    }.filter(_.isDefined).map(_.get).toList

    //Map which are in the repository but not in the model
    val m: List[(String, ComponentModel)] = Map(services.toList: _*).filter { case (_, service@Service(serviceName, _, _, _)) => model.get(serviceName).isEmpty }.map { case (_, service@Service(serviceName, _, _, _)) =>
      val newState = service.update(action, service.initialState).asInstanceOf[ComponentModel]
      (serviceName, newState)
    }.toList

    (nm ++ m).toMap
  }

  private[this] val servicesContextMapper = context.map(GenLens[CounterAppModel](_.services))
  private[this] val serviceContext = servicesContextMapper(serviceSubscriber)

  private[CounterApp] val services = WMap.empty[BaseComponent, Service[_ <: BaseComponent]]


  def service[A <: BaseComponent : Manifest]: A = {
    val m = manifest[A]

    val serviceName = m.runtimeClass.getName

    val oService = services.find {
      case (_, Service(`serviceName`, _, _, _)) => true
      case _ => false
    } map {
      _._2
    }

    oService.getOrElse {
      val sb2 = Services.services.getOrElse(serviceName, throw new IllegalArgumentException(s"There is no configured service with name: $serviceName"))
      if (!sb2.isInstanceOf[ServiceBuilder[A]]) throw new IllegalArgumentException(s"The service was not configured properly (the builder not belonging to the service): $serviceName")

      val sb = sb2.asInstanceOf[ServiceBuilder[A]]

      val serviceContext = new Context[A#ModelType] {
        override val changes: Observable[A#ModelType] = context.changes.map {
          _.services
        }.filter {
          _.contains(serviceName)
        }.map { in => in(serviceName).asInstanceOf[A#ModelType] }

        override val chain: UpdateChain[A#ModelType] = UpdateChain[A#ModelType]()

        override val channel: Observer[Action] = context.channel

        override val backgroundExecutor: ExecutionContext = context.backgroundExecutor
      }
      val (initialState, service) = sb(serviceContext)
      services(service) = Service(serviceName, serviceContext, initialState, service)
      services(service)
    }.service.asInstanceOf[A]
  }

}
