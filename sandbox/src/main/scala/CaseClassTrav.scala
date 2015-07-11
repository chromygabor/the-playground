/**
 * Created by chrogab on 2015.07.07..
 */
object CaseClassTrav extends App {
  trait Component
  trait ComponentModel {
    type Component
  }
  trait Model[C <: Component] extends ComponentModel {
    type Component = C
  }

  class MainComponent extends Component {
    def dummyMain() = {}
  }
  class SubComponent extends Component {
    def dummySub() = {}
  }

  case class MainModel(left: SubModel = SubModel(), right: SubModel = SubModel()) extends Model[MainComponent]
  case class SubModel(value: Int = 0) extends Model[SubComponent]



  def create[A <: ComponentModel with Product](implicit m1: Manifest[A], m2: Manifest[A#Component]): A#Component = {
    //def isModel[T](implicit ev: T <:< ComponentModel = null) = ev != null
    import scala.reflect.runtime.universe._
    import reflect.runtime.{currentMirror=>mirror}

    def isModel[M](t: Type): Boolean = {
      t.baseClasses.contains(typeOf[ComponentModel].typeSymbol)
    }

    def createComponent(rdType: Type): AnyRef = {
//      val values = Map("intVal"->5, "stringVal"->"five")
//      val rdClass = mirror.reflectClass(rdType.typeSymbol.asClass)
//      val constructors = rdType.members.filter(m=>m.isMethod && m.asMethod.isConstructor)
//      val defaultCtor = rdType.member(nme.CONSTRUCTOR)
//      val runtimeCtor = rdClass.reflectConstructor(defaultCtor.asMethod)
//
//      val paramsList = defaultCtor.asMethod.paramss
//
//      val mappedValues = params map (m=>values(m.name.toString))
//      val newRD = runtimeCtor.apply(mappedValues:_*)

      val rdClass = mirror.reflectClass(rdType.typeSymbol.asClass).symbol
      val ctor = rdClass.primaryConstructor

      val classMirror = mirror.reflectClass(rdType.typeSymbol.asClass)
      classMirror.reflectConstructor(ctor.asMethod).apply(2, "bar")

      ???
    }

    def echo(in: Type): Unit = {
      in.baseClasses.map { bc =>
        println(bc.typeSignature.members.filter(_.isType).filter(_.asType.isAliasType).map(_.typeSignature))
      }
    }

    val l = typeOf[A].members.filter(!_.isMethod).map(_.typeSignature).filter(isModel).map { in =>
      //createComponent(in)
      echo(in)
    }

    //println(l)

//    manifest[A].runtimeClass.getDeclaredFields.foreach { field =>
//      //println(field.getType.getInterfaces)
//
//      //      val l = field.getType.getInterfaces.toList.foreach { in =>
//      //        println(s" **** ${in.getName} ")
//      //      }
//      println(s"${field.getName} - ${field.getType} " )
//    }


    val comp = manifest[A#Component].runtimeClass.newInstance().asInstanceOf[A#Component]
    comp
  }

  val s = create[MainModel]
  s.dummyMain()

}