package com.chromy.frpui.javafxapp

import com.chromy.frpui.fw.core.{Context, Uid, Service}

/**
 * Created by cry on 2015.11.08..
 */
trait DialogService {

}

case class DialogServiceImpl(uid: Uid = Uid()) extends Service[DialogService, DialogServiceImpl] {
  override def api(context: Context): DialogService = new DialogService {
    
  }
}
