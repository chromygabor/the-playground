package com.chromy.frpui.fw.core

/**
 * Created by cry on 2015.11.01..
 */
trait Context {
  def getService[B : Manifest]: B

  def onAction(action: Event): Unit
}
