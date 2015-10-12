package com.chromy.reactiveui.core

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicReference

import com.chromy.reactiveui.core.misc.{Executable, SideChain}
import org.scalatest.FunSpecLike

import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration._


/**
 * Created by cry on 2015.09.22..
 */
class SideChainTest extends FunSpecLike {

  describe("SideChain") {
    it("should run on different thread") {
      val sc = SideChain[Int]

      val thread1: AtomicReference[String] = new AtomicReference[String]("")
      val thread2: AtomicReference[String] = new AtomicReference[String]("")


      val subscriber1: Int => Executable = { input =>
        Thread.sleep(100)
        thread1.set(Thread.currentThread.getName)
        Executable {
          Thread.sleep(100)
          thread2.set(Thread.currentThread.getName)
        }
      }
      sc.subscribe(subscriber1)

      val ec1 = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
      val ec2 = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

      val f = Future {
        sc.update(10)
      }(ec1).map({
        _.run()
      })(ec2)

      Await.ready(f, 1000 millis)

      assert(thread1.get() != thread2.get())

    }
    it("should filter by the input") {
      val sc = SideChain[Int]

      var list: List[Int] = Nil

      val subscriber1: Int => Executable = { input =>
        Executable {
          list = input :: list
        }
      }

      sc.filter(_ > 10).subscribe(subscriber1)

      sc.update(10).run()
      sc.update(11).run()
      sc.update(12).run()

      assert(List(12,11) == list)
    }

    it("should map input") {
      val sc = SideChain[Int]

      var list: List[Int] = Nil

      val subscriber1: Int => Executable = { input =>
        Executable {
          list = input :: list
        }
      }

      val sc2 = sc.map(_ * 2 )
      sc2.subscribe(subscriber1)

      sc.update(10).run()
      sc.update(11).run()
      sc.update(12).run()

      assert(List(24,22,20) == list)
    }

    it("should make distinguish") {
      val sc = SideChain[Int]

      var list: List[Int] = Nil

      val subscriber1: Int => Executable = { input =>
        Executable {
          list = input :: list
        }
      }

      val sc2 = sc.distinctUntilChanged
      sc2.subscribe(subscriber1)

      sc.update(10).run()
      sc.update(10).run()
      sc.update(11).run()
      sc.update(11).run()
      sc.update(12).run()

      assert(List(12,11,10) == list)
    }

    it("should handle errors somehow") {
      val sc = SideChain[Int]

      var list: List[Int] = Nil

      val subscriber1: Int => Executable = { input =>
        throw new Exception("Test exception")
        Executable {
          list = input :: list
        }
      }

      val subscriber2: Int => Executable = { input =>
        Executable {
          list = input :: list
        }
      }

      sc.subscribe(subscriber1)
      sc.subscribe(subscriber2)

      val r = sc.update(10)
      assert(r.errors.size == 1)
      r.run()
      assert(List(10) == list)
    }
  }

}
