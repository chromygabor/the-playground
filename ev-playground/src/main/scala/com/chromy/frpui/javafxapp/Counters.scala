package com.chromy.frpui.javafxapp

import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.layout.FlowPane

class CountersController  {
  @FXML private var _bAdd: Button = _
  lazy val bAdd = _bAdd

  @FXML private var _pCounters: FlowPane = _
  lazy val pCounters = _pCounters

  @FXML private var _counterNavigatorController: CounterNavigatorController = _
  lazy val counterNavigator = _counterNavigatorController

}
