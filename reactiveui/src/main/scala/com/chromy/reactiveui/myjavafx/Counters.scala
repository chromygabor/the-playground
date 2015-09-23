package com.chromy.reactiveui.myjavafx

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

import com.chromy.reactiveui.core._
import com.chromy.reactiveui.core.misc.Executable
import com.chromy.reactiveui.core.misc.Utils._
import monocle.macros.GenLens
import rx.lang.scala.{Observer, Subscriber}

import scala.util.{Failure, Success}


case class CountersModel(counters: List[CounterModel] = List(), counterNavigator: CounterNavigatorModel = CounterNavigatorModel(), uid: Uid = Uid()) extends Model[Counters]

case object Add extends Action


class Counters(protected val contextMapper: ContextMapper[CountersModel]) extends Component[CountersModel] {

  override def update(model: ModelType) = Simple {
    case Add =>
      model.copy(counters = CounterModel() :: model.counters)
    case Counter.Removed(uid) =>
      val newCounters = model.counters.filter {
        _.uid != uid
      }
      model.copy(counters = newCounters)
  }

  class ChildrenComponents {
    val counters = ListComponentOf[Counter](context.map(GenLens[CountersModel](_.counters)))((contextMapper, state) => new Counter(contextMapper))
    val counterNavigator = new CounterNavigator(context.map(GenLens[CountersModel](_.counterNavigator)))
  }

  val childrenComponents = new ChildrenComponents

}


class CountersController extends GenericJavaFXController[Counters] {

  @FXML private var _bAdd: Button = _
  lazy val bAdd = _bAdd

  @FXML private var _pCounters: FlowPane = _
  lazy val pCounters = _pCounters

  @FXML private var _counterNavigatorController: CounterNavigatorController = _
  lazy val counterNavigator = _counterNavigatorController

  private var _component: Counters = _

  lazy val subscriber: CountersModel => Executable =  { changes => Executable {
      bAdd.setOnAction { () => _component.channel.onNext(Add) }
    }

  }

  lazy val listSubscriber: List[Operation[Counter]] => Executable =  { changes => Executable {

      val moveItems = changes filter {
        case _: MoveItem[_] => true
        case _ => false
      } map { case in@MoveItem(item, originalIndex, _, _) =>
        (in, pCounters.getChildren.get(originalIndex))
      } reverse

      changes foreach {
        case MoveItem(_, _, computedIndex, _) =>
          pCounters.getChildren.remove(computedIndex)
        case DeleteItem(_, _, computedIndex) =>
          pCounters.getChildren.remove(computedIndex)
        case _ =>
      }

      changes foreach {
        case AddItem(component, index) =>
          JavaFXController[CounterController](component) match {
            case Success((parent, controller, _)) =>
              pCounters.getChildren.add(index, parent)
            case Failure(e) =>
              e.printStackTrace()
          }
        case _ =>
      }

      moveItems.foreach { case (MoveItem(_, _, _, newIndex), module) =>
        pCounters.getChildren.add(newIndex, module)
      }
    }

  }

  override def dispatch(component: Counters): Counters = {
    _component = component

    counterNavigator.dispatcher(component.childrenComponents.counterNavigator)

    _component.subscribe(subscriber)

    _component.childrenComponents.counters.subscribe(listSubscriber)
    _component
  }
}
