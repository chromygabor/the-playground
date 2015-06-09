package com.chromy.reactiveui

import org.scalatest.FunSpecLike
import rx.lang.scala.{Observable, Subject}

/**
 * Created by cry on 2015.05.26..
 */
class ObservableTest extends FunSpecLike {

  describe("observable") {
    it("should implement withLastItem") {
      val trigger = Subject[Int]
      val subject = Subject[Int]

      val v1: Observable[(Option[Int], Option[Int])] = trigger.map( in => (Some(in), None))
      val v2: Observable[(Option[Int], Option[Int])] = subject.map( in => (None, Some(in)))

      val m = v1.merge(v2)

      val scan = m.scan((None.asInstanceOf[Option[Int]], None.asInstanceOf[Option[Int]])){ (acc: (Option[Int], Option[Int]), act: (Option[Int], Option[Int])) =>
        if(act._2 != None ) {
          (act._2, act._1)
        } else {
          (acc._1, act._1)
        }
      }

      scan.filter(in => in._2 != None && in._1 != None).map{ in => (in._1.get, in._2.get)}.subscribe(
        {in => println(in)}
      )

      trigger.onNext(1)
      trigger.onNext(2)
      trigger.onNext(3)
      trigger.onNext(4)

      subject.onNext(10)
      subject.onNext(11)
      subject.onNext(12)
      subject.onNext(13)

      trigger.onNext(5)
      subject.onNext(14)
      trigger.onNext(0)
    }

  }

}
