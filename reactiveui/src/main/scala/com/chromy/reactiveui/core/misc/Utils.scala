package com.chromy.reactiveui.core.misc

import javafx.collections.ObservableList
import javafx.event.{ActionEvent, EventHandler}

import scala.collection.JavaConversions._


/**
 * Created by cry on 2015.04.17..
 */
object Utils {
  implicit def func2eventHandler(in: () => Unit): EventHandler[ActionEvent] = new EventHandler[ActionEvent] {
    override def handle(event: ActionEvent): Unit = in()
  }

  implicit def observableList2List[T](observableList: ObservableList[T]): List[T] = {
    val list: java.util.List[T] = observableList
    list.toList
  }
}
