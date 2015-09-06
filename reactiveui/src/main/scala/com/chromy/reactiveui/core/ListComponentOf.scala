package com.chromy.reactiveui.core


import com.chromy.reactiveui.core.misc.ListDiff
import rx.lang.scala.{Scheduler => ScalaScheduler, _}
import rx.schedulers.Schedulers

/**
 * Created by cry on 2015.08.04..
 */

object ListComponentOf {
  def apply[A <: BaseComponent{type ModelType <: BaseModel}](iContextMapper: ContextMapper[List[A#ModelType]])(iCreate: (Context[A#ModelType], A#ModelType) => A): ListComponentOf[A] = {
    val listComponentOf = new ListComponentOf[A] {
      override def contextMapper = iContextMapper

      override def create: (Context[A#ModelType], A#ModelType) => A = iCreate
    }
    listComponentOf
  }
}

/**
 * ListComponentOf
 * @tparam A
 */
trait ListComponentOf[A <: BaseComponent {type ModelType <: BaseModel}] extends BaseComponent {

  type ComponentType = A
  type ComponentModel = ComponentType#ModelType
  type ModelType = List[Operation[ComponentType]]

  protected def contextMapper: ContextMapper[List[ComponentModel]]

  protected def create: (Context[ComponentModel], ComponentModel) => ComponentType


  private lazy val name = s"ListComponentOf"
  println(s"[$name] created with")

  val stream = Subject[ModelType]

  private var _components = Map[Uid, ComponentType]()

  def childrenComponents = _components

  def childrenComponents_=(newComponents: Map[Uid, ComponentType]) = _components = newComponents


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
          DeleteItem(component, removedIndex, computedIndex)
      }
    }

    val insertItems = toInsert.filter { case (item, index) => toDelete.find { case (deletedItem, _) => deletedItem == item } match {
      case Some(_) => false
      case _ => true
      }
    }.map { case (item, index) =>
      val component = childrenComponents.getOrElse(item.uid, createComponent(item))
      childrenComponents = childrenComponents + (item.uid -> component)
      AddItem(component, index)
    }.toList

    stream.onNext(deleteItems ++ insertItems)

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
      override val changes: Observable[ComponentModel] = myChanges.filter {_.contains(item.uid)} map { change => change(item.uid)}
      override val chain: UpdateChain[ComponentModel] = UpdateChain[ComponentModel]
      override val channel: Observer[Action] = myContext.channel
      override val chainExecutor = myContext.chainExecutor
      override val changesExecutor = myContext.changesExecutor
    }, item)
  }

  val context = {
    new Context[ModelType] {
      override def changes: Observable[ModelType] = stream.sample(myContext.changes)

      override def chain: UpdateChain[ModelType] = {
        throw new IllegalStateException("This shouldn't be called because never registered")
      }

      override def channel: Observer[Action] = myContext.channel

      override val chainExecutor = myContext.chainExecutor
      override val changesExecutor = myContext.changesExecutor
    }
  }

  lazy val chainScheduler = new ScalaScheduler {
    val asJavaScheduler = Schedulers.from(context.chainExecutor)
  }

  lazy val changesScheduler = new ScalaScheduler {
    val asJavaScheduler = Schedulers.from(context.changesExecutor)
  }


  def subscribe(subscriber: Subscriber[List[Operation[ComponentType]]]) = {
    context.changes.observeOn(changesScheduler).subscribe(subscriber)
  }
}

sealed trait Operation[M] extends Model[ListComponentOf[_]] {
  override val uid = Uid()
}

case class DeleteItem[M](item: M, originalIndex: Int, computedIndex: Int) extends Operation[M]

case class AddItem[M](item: M, index: Int) extends Operation[M]

case class MoveItem[M](item: M, originalIndex: Int, computedIndex: Int, newIndex: Int) extends Operation[M]
