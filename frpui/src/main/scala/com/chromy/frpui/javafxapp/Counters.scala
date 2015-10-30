package com.chromy.frpui.javafxapp

import com.chromy.frpui.core._

/**
 * Created by cry on 2015.10.30..
 */
case class Counters(val uid: Uid = Uid()) extends Model[Counters]{
  override protected def handle(implicit context: Context): Updater[Counters] = ???
}

class CountersController extends Controller[Counters] {
  override val subscriber: (Counters) => SideEffect = ???
}
