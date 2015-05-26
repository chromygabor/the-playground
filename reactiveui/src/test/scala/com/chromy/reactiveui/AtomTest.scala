package com.chromy.reactiveui

import org.scalatest.FunSpecLike

/**
 * Created by cry on 2015.05.23..
 */
class AtomTest extends FunSpecLike {

  trait Change

  case class GrandChildModel(value: Int = 0) extends Change

  case class ChildModel(value: Int = 0, leftGrandChild: GrandChildModel = GrandChildModel(), rightGrandChild: GrandChildModel = GrandChildModel()) extends Change

  case class ParentModel(child1: ChildModel = ChildModel(), child2: ChildModel = ChildModel()) extends Change

  describe("Atom") {
    it("should be mappable and mapped atoms should propagate changes appropiatelly") {
      println("==============================")
      var changes = List[(ParentModel, ParentModel)]()
      val model = ParentModel()

      val root = Atom(model) {
        case (oldModel, newModel) =>
          changes = oldModel -> newModel :: changes
          println(s"$oldModel => $newModel")
      }

      //root.model.subscribe({in => println(in)})

      val child1Atom = root.map(_.child1)(newChild => _.copy(child1 = newChild))
      val child2Atom = root.map(_.child2)(newChild => _.copy(child2 = newChild))
      val leftGrandChildAtom = child1Atom.map(_.leftGrandChild)(newGrandChild => _.copy(leftGrandChild = newGrandChild))

      root.fire(model.copy(model.child1.copy(value = 1)))
      root.fire(model.copy(model.child1.copy(value = 2)))

      child1Atom.fire(ChildModel(3))
      child1Atom.fire(ChildModel(4))

      child2Atom.fire(ChildModel(5))
      child2Atom.fire(ChildModel(6))


      leftGrandChildAtom.fire(GrandChildModel(7))
      leftGrandChildAtom.fire(GrandChildModel(8))

      val expected = List(
        ParentModel(ChildModel(4, GrandChildModel(7), GrandChildModel(0)), ChildModel(6, GrandChildModel(0), GrandChildModel(0))) -> ParentModel(ChildModel(4, GrandChildModel(8), GrandChildModel(0)), ChildModel(6, GrandChildModel(0), GrandChildModel(0))),
        ParentModel(ChildModel(4, GrandChildModel(0), GrandChildModel(0)), ChildModel(6, GrandChildModel(0), GrandChildModel(0))) -> ParentModel(ChildModel(4, GrandChildModel(7), GrandChildModel(0)), ChildModel(6, GrandChildModel(0), GrandChildModel(0))),
        ParentModel(ChildModel(4, GrandChildModel(0), GrandChildModel(0)), ChildModel(5, GrandChildModel(0), GrandChildModel(0))) -> ParentModel(ChildModel(4, GrandChildModel(0), GrandChildModel(0)), ChildModel(6, GrandChildModel(0), GrandChildModel(0))),
        ParentModel(ChildModel(4, GrandChildModel(0), GrandChildModel(0)), ChildModel(0, GrandChildModel(0), GrandChildModel(0))) -> ParentModel(ChildModel(4, GrandChildModel(0), GrandChildModel(0)), ChildModel(5, GrandChildModel(0), GrandChildModel(0))),
        ParentModel(ChildModel(3, GrandChildModel(0), GrandChildModel(0)), ChildModel(0, GrandChildModel(0), GrandChildModel(0))) -> ParentModel(ChildModel(4, GrandChildModel(0), GrandChildModel(0)), ChildModel(0, GrandChildModel(0), GrandChildModel(0))),
        ParentModel(ChildModel(2, GrandChildModel(0), GrandChildModel(0)), ChildModel(0, GrandChildModel(0), GrandChildModel(0))) -> ParentModel(ChildModel(3, GrandChildModel(0), GrandChildModel(0)), ChildModel(0, GrandChildModel(0), GrandChildModel(0))),
        ParentModel(ChildModel(1, GrandChildModel(0), GrandChildModel(0)), ChildModel(0, GrandChildModel(0), GrandChildModel(0))) -> ParentModel(ChildModel(2, GrandChildModel(0), GrandChildModel(0)), ChildModel(0, GrandChildModel(0), GrandChildModel(0))),
        ParentModel(ChildModel(0, GrandChildModel(0), GrandChildModel(0)), ChildModel(0, GrandChildModel(0), GrandChildModel(0))) -> ParentModel(ChildModel(1, GrandChildModel(0), GrandChildModel(0)), ChildModel(0, GrandChildModel(0), GrandChildModel(0)))
      )

      assert(changes == expected)
    }
    it("should notify different subscribers about the appropiate changes") {
      println("==============================")
      var changes = List[(String, Change)]()
      val model = ParentModel()

      val root = Atom(model) {
        case (oldModel, newModel) =>
      }

      root.changes.subscribe({ model =>
        changes = "root" -> model :: changes
      })

      //      for( i <- 1 to 100000) {
      //        root.fire(model.copy(model.child1.copy(value = i)))
      //      }

      //root.fire(model.copy(model.child1.copy(value = 1)))

      val child1Atom = root.map(_.child1)(newChild => _.copy(child1 = newChild))

      child1Atom.changes.subscribe({ model =>
        changes = "child1" -> model :: changes
      })

      val child2Atom = root.map(_.child2)(newChild => _.copy(child2 = newChild))
      child2Atom.changes.subscribe({ model =>
        changes = "child2" -> model :: changes
      })

      val leftGrandChildAtom = child1Atom.map(_.leftGrandChild)(newGrandChild => _.copy(leftGrandChild = newGrandChild))
      leftGrandChildAtom.changes.subscribe({ model =>
        changes = "leftGrandChild" -> model :: changes
      })

      //      root.fire(model.copy(model.child1.copy(value = 2)))
      //
      child1Atom.fire(ChildModel(3))
      child1Atom.fire(ChildModel(3))
      child1Atom.fire(ChildModel(3))
      child1Atom.fire(ChildModel(4))
      child1Atom.fire(ChildModel(4))
      child1Atom.fire(ChildModel(4))
      child1Atom.fire(ChildModel(3))
      //
      //      child2Atom.fire(ChildModel(5))
      //      child2Atom.fire(ChildModel(6))

      leftGrandChildAtom.fire(GrandChildModel(7))
      //leftGrandChildAtom.fire(GrandChildModel(8))

      println(changes.reverse.mkString("\n"))
    }
    it("release the subscribers well") {

      val model = ParentModel()

      val root = Atom(model) {
        case (oldModel, newModel) =>
      }

    }
  }

}
