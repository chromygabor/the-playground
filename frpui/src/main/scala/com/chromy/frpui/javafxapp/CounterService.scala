package com.chromy.frpui.javafxapp

import com.chromy.frpui.fw.core.{Context, Service, Uid}

/**
 * Created by cry on 2015.11.06..
 */
trait CounterService
case class CounterServiceImpl(val uid: Uid = Uid()) extends Service[CounterService, CounterServiceImpl] {
  override def api(context: Context): CounterService = new CounterService {}
} 
