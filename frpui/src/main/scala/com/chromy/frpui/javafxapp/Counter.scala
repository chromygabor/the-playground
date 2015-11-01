package com.chromy.frpui.javafxapp

import com.chromy.frpui.Renderer
import com.chromy.frpui.fw.core.{Render, SideEffect, Model, Uid}
import com.chromy.frpui.fw.javafx.Controller

/**
 * Created by cry on 2015.10.30..
 */
case class Counter(uid: Uid = Uid()) extends Model[Counter] {
}

class CounterController extends Controller[Counter] {
//  override def subscriber: (Counter) => SideEffect = Render { model =>
//    SideEffect()
//  }

  override lazy val renderer: Renderer[Counter] = Renderer()
}
