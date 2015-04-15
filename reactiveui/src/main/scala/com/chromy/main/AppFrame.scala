package com.chromy.main

import javafx.scene.layout.AnchorPane

import com.chromy.util.FXMLController

/**
 * Created by cry on 2015.04.12..
 */
class AppFrame extends AnchorPane with FXMLController {
  val behavior = new AppFrameBehavior

}
