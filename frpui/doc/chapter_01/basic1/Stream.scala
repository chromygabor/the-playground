package com.chromy.frpui.examples.basics.basic1

import rx.lang.scala.Subject

/**
 * Created by cry on 2015.10.21..
 */
object Stream extends App{
  val s = Subject[String]()

  s.scan("Z"){ (accu, item) =>
    accu + item
  }.foreach { model =>
    println(model)
  }


  s.onNext("A")
  s.onNext("B")
  s.onNext("C")
  s.onNext("D")
}
