package com.chromy.frpui.fw.core

import java.util.concurrent.atomic.AtomicReference

/**
 * Created by cry on 2015.09.22..
 */

@Deprecated
object Render {
  def apply[A](f: A => SideEffect): (A => SideEffect) = f
  def scan[A](init: A)( f: (A,A) => SideEffect): (A => SideEffect) = {
    val prevValue = new AtomicReference[A]()
    val res: A => SideEffect = { in =>
      val res = if(in != prevValue.get) {
        val oldValue = prevValue.get
        prevValue.set(in)
        f(oldValue, in)
      } else {
        SideEffect()
      }
      res
    }
    res
  }
}
