package com.chromy.frpui.fw.core

/**
 * Created by cry on 2015.11.01..
 */


trait Behavior[M <: BaseModel] {
  def action(f: (M, UpdateContext) => Result[M]): M => Action[M] = {model => Action[M](model) (f) }
}
