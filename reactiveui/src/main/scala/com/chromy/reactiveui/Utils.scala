package com.chromy.reactiveui

import java.awt.Toolkit
import java.util.UUID
import java.util.function.{Consumer, Predicate}
import javafx.application.{Platform, Application}
import javafx.collections.ObservableList
import javafx.embed.swing.JFXPanel
import javafx.event.{ActionEvent, EventHandler}
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Node, Scene}
import javafx.stage.Stage
import rx.lang.scala.{Subject, Observable}

/**
 * Created by cry on 2015.04.17..
 */
object Utils {
  implicit def func2eventHandler(in: () => Unit): EventHandler[ActionEvent] = new EventHandler[ActionEvent] {
    override def handle(event: ActionEvent): Unit = in()
  }
}
