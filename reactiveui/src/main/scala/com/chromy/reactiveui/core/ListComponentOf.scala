package com.chromy.reactiveui.core


import com.chromy.reactiveui.core.misc.ListDiff
import rx.lang.scala.{Subscriber, Observable, Observer, Subject}

/**
 * Created by cry on 2015.08.04..
 */

case class Changes[A](deleteItems: List[A], insertItems: List[A], uid: Uid = Uid()) extends BaseModel

object ListComponentOf {
  def apply[A <: BaseComponent{type ModelType <: BaseModel}](iRouterMapper: RouterMapper[List[A#ModelType]])(iCreate: (Router[A#ModelType], A#ModelType) => A): ListComponentOf[A] = {
    val listComponentOf = new ListComponentOf[A] {
      override def routerMapper = iRouterMapper

      override def create: (Router[A#ModelType], A#ModelType) => A = iCreate
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

  protected def routerMapper: RouterMapper[List[ComponentModel]]

  protected def create: (Router[ComponentModel], ComponentModel) => ComponentType


  private lazy val name = s"ListComponentOf"
  println(s"[$name] created with")

  val stream = Subject[ModelType]

  private var _components = Map[Uid, ComponentType]()

  def childrenComponents = _components

  def childrenComponents_=(newComponents: Map[Uid, ComponentType]) = _components = newComponents


  val myRouter = routerMapper { (action, original, state) =>
    println(s"[$name] a new state was requested for $original and $action")

    val (toDelete, toInsert) = ListDiff.diff(original, state) {
      _.uid.uid.toString
    }

    val deleteItems = toDelete.foldLeft((0, List[(ComponentModel, Int)]())) { case ((removedItems, list), (item, index)) =>
      val newItem = item -> (index - removedItems)
      (removedItems + 1, newItem :: list)
    }._2.map { case (item, index) =>
      val component = childrenComponents(item.uid)
      toInsert.find { case (model, _) => model.uid == item.uid} match {
        case Some(_) =>
        case None =>
          childrenComponents = childrenComponents - item.uid
      }
      DeleteItem(component, index)
    }

    val insertItems = toInsert.map { case (item, index) =>

      val component = childrenComponents.getOrElse(item.uid, {
        println(s"[$name] creating component with: $item")
        createComponent(item)
      })
      childrenComponents = childrenComponents + (item.uid -> component)
      AddItem(component, index)
    }.toList

    stream.onNext(deleteItems ++ insertItems)

    state.map { in =>
      childrenComponents.get(in.uid) match {
        case Some(component) => component.router.chain.update(action, in.asInstanceOf[component.ModelType])
        case _ => in
      }
    }
  }


  private val myChanges = myRouter.changes.map(_.map { in => in.uid -> in }.toMap)

  def createComponent(item: ComponentModel): ComponentType = {
    create(new Router[ComponentModel] {
      override val changes: Observable[ComponentModel] = myChanges.filter {_.contains(item.uid)} map { change => change(item.uid)}
      override val chain: UpdateChain[ComponentModel] = UpdateChain[ComponentModel]
      override val channel: Observer[Action] = myRouter.channel
    }, item)
  }

  val router = {
    new Router[ModelType] {
      override def changes: Observable[ModelType] = stream.sample(myRouter.changes)

      override def chain: UpdateChain[ModelType] = {
        throw new IllegalStateException("This shouldn't be called because never registered")
      }

      override def channel: Observer[Action] = myRouter.channel
    }
  }

  def subscribe(subscriber: Subscriber[List[Operation[ComponentType]]]) = {
    router.changes.subscribe(subscriber)
  }
}

sealed trait Operation[M] extends Model[ListComponentOf[_]] {
  override val uid = Uid()
}

case class DeleteItem[M](item: M, index: Int) extends Operation[M]

case class AddItem[M](item: M, index: Int) extends Operation[M]
