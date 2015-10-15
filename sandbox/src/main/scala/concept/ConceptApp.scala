package concept

import monocle.Lens
import monocle.macros.GenLens

/**
 * Created by cry on 2015.10.15..
 */

object ConceptApp extends App {

  trait Action

  case object TestAction extends Action

  trait BaseModel {
    type M <: BaseModel

    case class Child[B <: BaseModel](lens: Lens[M, B]) {
      def step(action: Action, model: M): M = {
        val newModel = lens.get(model).step(action)
        lens.set(newModel.asInstanceOf[B])(model)
      }
    }

    val children: List[Child[_]] = Nil

    def step(action: Action): M = {
      val newModel = children.foldLeft(this) { (model, child) =>
        child.step(action, model.asInstanceOf[M])
      }
      newModel.asInstanceOf[M]
    }
  }

  trait Model[B <: BaseModel] extends BaseModel {
    type M = B
  }

  trait SubModel extends Model[SubModel] {
    override def step(action: Action): this.type = {
      println(s"Submodel step was called with: $action")
      this
    }
  }

  case class LeftSubmodel(value: Int = 0) extends SubModel

  case class RightSubmodel(value: Int = 0) extends SubModel

  case class MainModel(left: LeftSubmodel = LeftSubmodel(), right: RightSubmodel = RightSubmodel()) extends Model[MainModel] {


    override val children = List(
      Child(GenLens[MainModel](_.left)),
      Child(GenLens[MainModel](_.right))
    )

  }

  MainModel().step(TestAction)
}
