package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}

/**
 * Created by cry on 2015.10.30..
 */

class CounterNavigatorController  {
  @FXML var _bNext: Button = _
  lazy val bNext = _bNext
  @FXML var _bPrev: Button = _
  lazy val bPrev = _bPrev
  @FXML var _lValue: Label = _
  lazy val lValue = _lValue
  
}
