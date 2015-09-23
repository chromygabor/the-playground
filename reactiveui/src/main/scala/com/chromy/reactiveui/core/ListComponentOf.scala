package com.chromy.reactiveui.core


import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

import com.chromy.reactiveui.core.misc.{Executable, ListDiff}
import rx.lang.scala.{Scheduler => ScalaScheduler, _}
import rx.schedulers.Schedulers

import scala.concurrent.ExecutionContext

/**
 * Created by cry on 2015.08.04..
 */

object ListComponentOf {
  def apply[A <: UiComponent](iContextMapper: ContextMapper[List[A#ModelType]])(iCreate: (ContextMapper[A#ModelType], A#ModelType) => A): ListComponentOf[A] = {
    val listComponentOf = new ListComponentOf[A] {
      override def contextMapper = iContextMapper

      override def create: (ContextMapper[A#ModelType], A#ModelType) => A = iCreate
    }
    listComponentOf
  }
}

/**
 * ListComponentOf
 * @tparam A
 */
trait ListComponentOf[A <: UiComponent] extends SimpleComponent {

  type ComponentType = A
  type ComponentModel = ComponentType#ModelType
  type ModelType = List[Operation[ComponentType]]

  protected def contextMapper: ContextMapper[List[ComponentModel]]

  protected def create: (ContextMapper[ComponentModel], ComponentModel) => ComponentType

  private lazy val name = s"ListComponentOf"
  println(s"[$name] created with")

  val queue = new ConcurrentLinkedQueue[List[Operation[ComponentType]]]()

  val stream = Subject[ModelType]

  private val _components = new AtomicReference[Map[Uid, ComponentType]](Map())

  def childrenComponents = _components.get()

  def childrenComponents_=(newComponents: Map[Uid, ComponentType]) = _components.set(newComponents)


  private val subscriber: (Action, List[ComponentModel], List[ComponentModel]) => List[ComponentModel] = { (action, original, state) =>
    println(s"[$name] a new state was requested for $original and $action")

    val (_toDelete, _toInsert) = ListDiff.diff(original, state) {
      _.uid.uid.toString
    }

    val (toDelete, toInsert) = (_toDelete.toIndexedSeq, _toInsert.toIndexedSeq)

    val (_, list) = toDelete.foldLeft((0, List.empty[(ComponentModel, Int, Int, Option[Int])])) { case ((numberOfRemovedItems, items), (item, removedIndex)) =>
      toInsert.find { case (itemInsert, insertedIndex) => itemInsert == item } match {
        case Some((_, insertedIndex)) =>
          val newItem = (item, removedIndex, removedIndex - numberOfRemovedItems, Some(insertedIndex))
          (numberOfRemovedItems + 1, newItem :: items)
        case None =>
          val newItem = (item, removedIndex, removedIndex - numberOfRemovedItems, None)
          (numberOfRemovedItems + 1, newItem :: items)
      }
    }

    val deleteItems = list.map { case (item, removedIndex, computedIndex, oInsertedIndex) =>
      val component = childrenComponents(item.uid)
      oInsertedIndex match {
        case Some(insertedIndex) =>
          MoveItem(component, removedIndex, computedIndex, insertedIndex)
        case _ =>
          childrenComponents = childrenComponents - item.uid
          DeleteItem(component, removedIndex, computedIndex)
      }
    }

    val insertItems = toInsert.filter { case (item, index) => toDelete.find { case (deletedItem, _) => deletedItem == item } match {
      case Some(_) => false
      case _ => true
      }
    }.map { case (item, index) =>
      val component = childrenComponents.getOrElse(item.uid, {
        val newComponent = createComponent(item)
        childrenComponents = childrenComponents + (item.uid -> newComponent)
        newComponent
      })
      AddItem(component, index)
    }.toList

    queue.add(deleteItems ++ insertItems)

    state.map { in =>
      childrenComponents.get(in.uid) match {
        case Some(component) => component.context.chain.update(action, in.asInstanceOf[component.ModelType])
        case _ => in
      }
    }
  }

  val myContext = contextMapper(subscriber)

  private val myChanges = myContext.changes.map(_.map { in => in.uid -> in }.toMap)

  def createComponent(item: ComponentModel): ComponentType = {
    create(new Context[ComponentModel] {
      override val changes = myChanges.filter {_.contains(item.uid)} map { change => change(item.uid)}
      override val chain: UpdateChain[ComponentModel] = UpdateChain[ComponentModel]
      override val channel: Observer[Action] = myContext.channel
      override val backgroundExecutor = myContext.backgroundExecutor
      override val initialState = item
    }.mapper, item)
  }

  val context = {
    new Context[ModelType] {
      override def changes = myContext.changes.map { _ => queue.poll() }
      override def chain: UpdateChain[ModelType] = {
        throw new IllegalStateException("This shouldn't be called because never registered")
      }
      override def channel: Observer[Action] = myContext.channel
      override val backgroundExecutor = myContext.backgroundExecutor
      override val initialState = Nil
    }
  }

//  def subscribe(subscriber: Subscriber[List[Operation[ComponentType]]]) = {
//    context.changes.distinctUntilChanged.filter(!_.isEmpty).subscribe(subscriber)
//  }
  def subscribe(subscriber: List[Operation[ComponentType]] => Executable) = {
    context.changes.distinctUntilChanged.filter(!_.isEmpty).subscribe(subscriber)
  }
}

sealed trait Operation[M <: UiComponent]

case class DeleteItem[M <: UiComponent](item: M, originalIndex: Int, computedIndex: Int) extends Operation[M]

case class AddItem[M <: UiComponent](item: M, index: Int) extends Operation[M]

case class MoveItem[M <: UiComponent](item: M, originalIndex: Int, computedIndex: Int, newIndex: Int) extends Operation[M]
