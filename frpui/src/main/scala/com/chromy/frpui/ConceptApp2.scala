package com.chromy.frpui

import java.util.concurrent.atomic.AtomicReference

import com.chromy.frpui.core._
import monocle.macros.GenLens
import rx.lang.scala.Subject

/**
 * Created by cry on 2015.10.15..
 */

trait Service[M] extends Model[M]

object ConceptApp2 extends App {
  case class MainModel(services: Map[Uid, Service] = Map(), uid: Uid = Uid()) extends Model[MainModel] {
    override def children = MainModel.children
  }

  object MainModel {
    val children = List(
      Child(GenLens[MainModel](_.services))
    )
  }

  val s = Subject[Action]
  val initialModel = MainModel()
  val render = SideEffectChain[MainModel]()

  val stream = s.scan(initialModel) { (model, action) =>
    val lServices = new AtomicReference[Map[Uid, Service]](Map())
    
    def getService[B](serviceName: String): B = {
      ???
    }
    val newModel = model.step(action)
    newModel.copy(services)
  }.subscribe({ model =>
    render.update(model).run()
  })

  val u1 = Uid()
//  s.onNext(AddItem(u1))
//  s.onNext(IncrementValue(2, u1))
//  s.onNext(IncrementValue(5, u1))

  val u2 = Uid()
//  s.onNext(AddValue(u2))
//  s.onNext(IncrementValue(7, u2))
//  s.onNext(IncrementValue(9, u2))
//  s.onNext(IncrementValue(11, u2))

}
