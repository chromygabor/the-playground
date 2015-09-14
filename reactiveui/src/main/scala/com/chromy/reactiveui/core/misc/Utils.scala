package com.chromy.reactiveui.core.misc

import javafx.collections.ObservableList
import javafx.event.{Event, EventHandler}

import scala.collection.JavaConversions._


/**
 * Created by cry on 2015.04.17..
 */
object Utils {
  implicit def func2eventHandler[B <: Event](in: () => Unit): EventHandler[B] = new EventHandler[B] {
    override def handle(event: B): Unit = in()
  }

  implicit def observableList2List[T](observableList: ObservableList[T]): List[T] = {
    val list: java.util.List[T] = observableList
    list.toList
  }
}
