package ui

import java.util.concurrent.Executor
import javafx.application.Platform
import javafx.embed.swing.JFXPanel

/**
  * Created by Gábor on 2016.09.30..
  */
//===========================================================================
//======================== Application part =================================
//===========================================================================
object JavaFXExecutor extends Executor {
  new JFXPanel

  override def execute(command: Runnable): Unit = Platform.runLater(command)
}
