package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}


/**
 * Created by cry on 2015.10.30..
 */



class CounterController {
  @FXML private var _lblCounter: Label = _
  @FXML private var _btnIncrement: Button = _
  @FXML private var _btnDecrement: Button = _
  @FXML private var _btnClose: Button = _

  lazy val lblCounter = _lblCounter
  lazy val btnIncrement = _btnIncrement
  lazy val btnDecrement = _btnDecrement
  lazy val btnClose = _btnClose


}
