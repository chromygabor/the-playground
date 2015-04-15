package com.msci.graph.ui.simple.fx.rxapp

import java.util.concurrent.Executor
import javafx.application.Platform
import javafx.beans.property.SimpleObjectProperty
import javafx.beans.value.ObservableValue
import javafx.collections.FXCollections
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.Node
import javafx.scene.control.TableColumn.CellDataFeatures
import javafx.scene.control._
import javafx.scene.layout.AnchorPane
import javafx.util.Callback

import rx.lang.scala.{Scheduler => ScalaScheduler, _}
import rx.lang.scala.schedulers.{NewThreadScheduler, ComputationScheduler, IOScheduler}
import rx.lang.scala.subjects.SerializedSubject
import rx.schedulers.Schedulers
import rx.subjects.PublishSubject

import javafx.scene.Node

/**
 * Created by chrogab on 2015.04.01..
 */


object RxNode {
  //  def apply(node: ButtonBase): RxNode = ???
  //  new RxNode {
  ////    node.setOnAction(new EventHandler[ActionEvent] {
  ////      override def handle(event: ActionEvent): Unit = {
  ////        println("click sent: " + Thread.currentThread().getName )
  ////        this.onNext(event)
  ////      }
  ////    })
  //  }
}

trait CellFactoryElem[S]

case class UpdateCell[S](updateItem: (TableCell[S, S], S) => CellFactoryValueElem)

case class ClearCell[S](updateItem: (TableCell[S, S]) => Unit)

sealed trait CellFactoryValueElem

case class Text(text: String) extends CellFactoryValueElem

case class Graphic(node: Node) extends CellFactoryValueElem

case class MyTableColumn[S](columnFactory: ColumnFactory[S], updateCell: UpdateCell[S], clearCell: ClearCell[S])

trait ColumnFactory[S] {
  def create(): TableColumn[S, S]
}

case class TextColumn[S](label: String) extends ColumnFactory[S] {
  override def create() = {
    new TableColumn[S, S](label)
  }
}

trait CellFactory[S] {
  def create(): TableCell[S, S]
}

object SwingScheduler {
  def apply() = {
    new ScalaScheduler {
      val asJavaScheduler = Schedulers.from(new Executor {
        override def execute(command: Runnable): Unit = Platform.runLater(command)
      })
    }
  }
}

class RxTableView[S](table: TableView[S]) {

  import scala.collection.JavaConversions._

  val columns = Subject[Seq[MyTableColumn[S]]]
  val items = Subject[S]()
  val allItems = Subject[Seq[S]]()

  allItems.observeOn(SwingScheduler()).subscribe { value =>
    println("allItems on: " + Thread.currentThread().getName)
    table.getItems.clear()
    table.getItems.addAll(value)
  }

  items.observeOn(SwingScheduler()).subscribe { value =>
    println("items on: " + Thread.currentThread().getName)
    table.getItems.add(value)
  }

  columns.observeOn(SwingScheduler()).subscribe { value =>
    table.getColumns.clear()
    table.getItems.clear()
    val columns = value.map { case MyTableColumn(columnFactory, updateCell, clearCell) =>
      val column = columnFactory.create()
      column.setCellValueFactory(new Callback[CellDataFeatures[S, S], ObservableValue[S]]() {
        override def call(param: CellDataFeatures[S, S]): ObservableValue[S] = new SimpleObjectProperty(param.getValue)
      })
      column.setCellFactory(new Callback[TableColumn[S, S], TableCell[S, S]] {
        override def call(param: TableColumn[S, S]): TableCell[S, S] = new TableCell[S, S] {
          var styles: String = ""

          override final def updateItem(item: S, empty: Boolean) {
            if (item == getItem()) return
            super.updateItem(item, empty)
            if (item == null) {
              clearCell.updateItem(this)
              setText(null)
              setGraphic(null)
            } else {
              updateCell.updateItem(this, item) match {
                case Text(text) => setText(text)
                case Graphic(node) => setGraphic(node)
              }
            }
          }
        }
      })
      column
    }
    table.getColumns.setAll(columns)
  }

}


case class Word(word: String)

case object ClickEvent extends Event

case object TestEvent extends Event

case class TestEvent2() extends Event

class RxAppController(view: RxAppView, behavior: RxAppBehavior) {

  import scala.collection.JavaConversions._
  import scala.io.Source

  val c1 = MyTableColumn[Word](TextColumn("First"), UpdateCell { case (cell, Word(word)) => Text(word) }, ClearCell { case (cell) => })
  val c2 = MyTableColumn[Word](TextColumn("Second"), UpdateCell { case (cell, Word(word)) => Text(word + " aaa ") }, ClearCell { case (cell) => })

  val rxTable = new RxTableView(view.table1)
  rxTable.columns.onNext(Seq(c1, c2))

  //val button = RxNode(view.button)

  def words = {
    println("Loading on: " + Thread.currentThread().getName)
    val o = Option {
      getClass.getResourceAsStream("words.txt")
    }.map { s => Source.fromInputStream(s).getLines().map { in => Word(in) }.toList }

    o match {
      case Some(res) => res
      case _ => Nil
    }
  }

  val button = new RxNode {
    view.button.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        println("click sent: " + Thread.currentThread().getName)
        publish(ClickEvent)
      }
    })

    reactors += {
      case e => println("reactor: " + e)
    }

    from[TestEvent2].subscribe { event =>
      println("from[TestEvent2] stream: " + event)
    }
  }

  button.subscribe { event =>
    println("button send: " + event)
    button.tell(TestEvent2())
  }


  val es = button.observeOn(NewThreadScheduler()).filter {
    case ClickEvent => true
    case _ => false
  } map {
    _.asInstanceOf[ClickEvent.type]
  }

  val wordsStream = es.map { _ =>
    println("Load starting on: " + Thread.currentThread().getName)
    val w = words
    println("Load stopping on: " + Thread.currentThread().getName)
    w
  }

  wordsStream.subscribe { words =>
    rxTable.allItems.onNext(words)
  }
}