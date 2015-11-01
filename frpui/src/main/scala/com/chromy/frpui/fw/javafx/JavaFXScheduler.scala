package com.chromy.frpui.fw.javafx

import java.util.concurrent.Executor
import javafx.application.Platform

import rx.lang.scala.{Scheduler => ScalaScheduler}
import rx.schedulers.Schedulers

/**
 * Created by chrogab on 2015.10.30..
 */

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
