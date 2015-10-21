package com.chromy.frpui.examples.basics

/**
 * Created by cry on 2015.10.21..
 */
object Basics extends App {
  val list = List("A", "B", "C", "D")

  val result = list.scanLeft("Z") { (accu, item) =>
    accu + item
  } foreach { model =>
    println(model)
  }


}
