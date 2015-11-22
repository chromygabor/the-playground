package com.chromy.frpui.javafxapp

import com.chromy.frpui.fw.core.{Service, Uid, UpdateContext}

/**
 * Created by cry on 2015.11.08..
 */
trait DialogService {

}

case class DialogServiceImpl(uid: Uid = Uid()) extends Service[DialogService, DialogServiceImpl] {
  override def api(context: UpdateContext): DialogService = new DialogService {
    
  }
}
