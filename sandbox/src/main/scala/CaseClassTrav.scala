//compile 'org.scala-lang:scala-reflect:2.11.6'
import scala.reflect.runtime.universe._
import reflect.runtime.{currentMirror=>mirror}

/**
 * Created by chrogab on 2015.07.07..
 */
object CaseClassTrav extends App {
  trait Component
  trait Model[C <: Component] {
    type Component = C
  }

  class MainComponent(param: Int) extends Component {
    def dummyMain() = {}
  }
  class SubComponent(param: Int) extends Component {
    println(s"param: $param")
    def dummySub() = {}
  }

  case class MainModel(left: SubModel = SubModel(), right: SubModel = SubModel()) extends Model[MainComponent]
  case class SubModel(value: Int = 0) extends Model[SubComponent]

  trait ComponentFactory {
    def apply(param: Int): Component
  }

  def isModel(t: Type): Boolean = {
    t.baseClasses.contains(typeOf[Model[_]].typeSymbol)
  }

  def traverseModel(tModel: Type): Map[String, ComponentFactory] = {
    tModel.members.filter(!_.isMethod).map(in => in.name ->  in.typeSignature).filter(in => isModel(in._2)).map { in =>
      in._1.toString -> createComponentFactoryForModel(in._2)
    }.toMap
  }

  def createComponentFactoryForModel(tModel: Type): ComponentFactory = {
    val modelClass = typeOf[Model[_]].typeSymbol.asClass
    val modelType = modelClass.typeParams(0).asType.toType
    val rdType = modelType.asSeenFrom(tModel, modelClass)
    val ctor = rdType.typeSymbol.asClass.primaryConstructor

    val classMirror = mirror.reflectClass(rdType.typeSymbol.asClass)
    new ComponentFactory {
      override def apply(param: Int): Component =
        classMirror.reflectConstructor(ctor.asMethod).apply(param).asInstanceOf[Component]
    }
  }

  //println(traverseModel(typeOf[MainModel]))

  case object MyTestObject

  private def companionOf(companionName: String): Any = {
    import scala.reflect.runtime.{currentMirror => cm}

    val moduleSymbol = cm.staticModule(companionName)
    val moduleMirror = cm.reflectModule(moduleSymbol)
    moduleMirror.instance
  }

  println(companionOf("CaseClassTrav.MyTestObject"))
}
