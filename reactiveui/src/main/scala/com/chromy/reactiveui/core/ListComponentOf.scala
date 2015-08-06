package com.chromy.reactiveui.core


import com.chromy.reactiveui.core.misc.ListDiff
import rx.lang.scala.{Observable, Observer, Subject}

/**
 * Created by cry on 2015.08.04..
 */
object ListComponentOf {
  def apply[M <: Model[_ <: Component]](iRouterMapper: RouterMapper[List[M]])(iCreate: (Router[M] => Component {type ModelType = M})): ListComponentOf[M] = {
    val listComponentOf = new ListComponentOf[M] {
      override def routerMapper = iRouterMapper

      override def create: (Router[M]) => Component {type ModelType = M} = iCreate

    }
    listComponentOf
  }
}

/**
 * ListComponentOf
 * @tparam M
 */
trait ListComponentOf[M <: Model[_ <: Component]] extends Component {

  //case class ListCompononentOfModel(routersToAdd: Map[String, (Int, ComponentType)], routersToRemove: Map[String, (Int, ComponentType)], uid: Uid = Uid()) extends Model[ListComponentOf[M]]

  type ComponentType = Component {type ModelType = M}
  type ModelType = Operation[ComponentType]

  protected def routerMapper: RouterMapper[List[M]]

  protected def create: Router[M] => ComponentType


  private lazy val name = s"ListComponentOf"
  println(s"[$name] created with")

  val stream = Subject[ModelType]

  private var _components = Map[Uid, ComponentType]()

  def childrenComponents = _components

  def childrenComponents_=(newComponents: Map[Uid, ComponentType]) = _components = newComponents


  val myRouter = {
    routerMapper { (action, original, state) =>
      println(s"[$name] a new state was requested for $original and $action")

      val (toDelete, toInsert) = ListDiff.diff(original, state) {
        _.uid.uid.toString
      }

      toDelete.foldLeft((0, List[(M, Int)]())) { case ((removedItems, list), (item, index)) =>
        val newItem = item -> (index - removedItems)
        (removedItems + 1, newItem :: list)
      }._2.foreach { case (item, index) =>
        val component = childrenComponents(item.uid)
        childrenComponents = childrenComponents - item.uid
        stream.onNext(DeleteItem(component, index))
      }

      toInsert.foreach { case (item, index) =>
        val component = createComponent(item)
        childrenComponents = childrenComponents + (item.uid -> component)
        stream.onNext(AddItem(component, index))
      }

      state.map { in =>
        childrenComponents.get(in.uid) match {
          case Some(component) => component.router.chain.update(action, in)
          case _ => in
        }
      }
    }
  }

  private val myChanges = myRouter.changes.map(_.map { in => in.uid -> in }.toMap)

  def createComponent(item: M): ComponentType = {
    create(new Router[M] {
      override val changes: Observable[M] = myChanges.map { change => change(item.uid) }
      override val chain: UpdateChain[M] = UpdateChain[M]
      override val channel: Observer[Action] = myRouter.channel
    })
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
}

sealed trait Operation[M] extends Model[ListComponentOf[_]] {
  override val uid = Uid()
}

case class DeleteItem[M](item: M, index: Int) extends Operation[M]

case class AddItem[M](item: M, index: Int) extends Operation[M]
