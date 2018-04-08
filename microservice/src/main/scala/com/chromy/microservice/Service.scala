package com.chromy.microservice

import akka.http.scaladsl.marshalling.{Marshal, ToEntityMarshaller}
import akka.http.scaladsl.model.{HttpMethod, MessageEntity}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller

import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

trait ServiceFactory[S <: Service] {
  def apply(invoker: Invoker): S
}
case class Call[Req, Res](invoke: Req => Future[Res])

case class ParamInfo[P](name: String, clazz: Class[P], serializer: ParamSerializer[P])

case class CallInfo[Req, Res](method: HttpMethod,
                              serviceName: String,
                              path: String,
                              methodInfo: MethodInfo[Req, Res],
                              fromReq: FromEntityUnmarshaller[Req],
                              toReq: ToEntityMarshaller[Req],
                              fromRes: FromEntityUnmarshaller[Res],
                              toRes: ToEntityMarshaller[Res],
                              clazzTag: ClassTag[Res]) {

  def invoke(s: Any, request: Any, paramValues: Seq[(String, Any)])(implicit ec: ExecutionContext): Future[MessageEntity] = {
    val method = methodInfo.method
    val t = paramValues.toMap
    val pv = methodInfo.params.map { paramInfo =>
      t(paramInfo.name).asInstanceOf[Object]

    }

    val c = method.invoke(s, pv: _*).asInstanceOf[Call[Req, Res]]
    c.invoke(request.asInstanceOf[Req]).asInstanceOf[Future[Res]].flatMap { res =>
      Marshal(res).to[MessageEntity](toRes, ec)
    }
  }
}

case class ServiceDescriptor(name: String, calls: Seq[CallInfo[_, _]] = Seq.empty) {
  def withRestCall[Req, Res](method: HttpMethod,
                             path: String,
                             methodInfo: MethodInfo[Req, Res])
                            (implicit fromReq: FromEntityUnmarshaller[Req],
                             fromRes: FromEntityUnmarshaller[Res],
                             toReq: ToEntityMarshaller[Req],
                             toRes: ToEntityMarshaller[Res],
                             resClazzTag: ClassTag[Res]
                            ): ServiceDescriptor = {
    val callInfo = CallInfo(method,
      name,
      path,
      methodInfo,
      fromReq,
      toReq,
      fromRes,
      toRes,
      resClazzTag)
    copy(calls = calls :+ callInfo)
  }
}



case class ServiceRegistry(private val invoker: Invoker) {
  def of[S <: Service](implicit serviceFactory: ServiceFactory[S]): S = {
    serviceFactory.apply(invoker)
  }

  def server[S <: Service : ClassTag](service: S)(implicit serviceFactory: ServiceFactory[S]): ServiceServerDescriptor[S] = {
    //val service = serviceFactory.apply(NoOpInvoker)
    ServiceServerDescriptor[S](service)
  }
}

trait Service {

  type NotUsed = NotUsed.type

  protected def descriptor: ServiceDescriptor

  private[microservice] def serviceDescriptor = descriptor
}

trait ServiceFacadeFactory[S] {
  def apply(): S
}

object Service {
  def facade[S <: Service]: S = macro facade_impl[S]


  def facade_impl[S <: Service : c.WeakTypeTag](c: blackbox.Context): c.Expr[S] = {
    import c.universe._
    val serviceTP = weakTypeTag[S]


    val invokerName = TermName("invoker")
    val invokerType = TypeName("Invoker")
    val invokeFn = TermName("invoke")

    val qt = c.typecheck(tq"com.chromy.microservice.Call[_, _]").tpe

    val s: Iterable[Tree] = serviceTP.tpe.decls.filter {
      case e if e.typeSignature.finalResultType.typeSymbol == qt.typeSymbol => true
      case _ => false
    }.map { decl =>
      val returnType = decl.typeSignature.finalResultType
      val reqTpe = returnType.typeArgs(0)
      val resTpe = returnType.typeArgs(1)

      def impl(methodName: TermName, paramNames: List[List[TermName]], paramTypes: List[Tree]) = {
        val methodNameConstant = Literal(Constant(methodName.toString))
        val descriptorTermName = TermName("descriptor")

        val res =
          q"""
            val callInfo = $descriptorTermName.calls.find { callInfo =>
             callInfo.methodInfo.method.getName == $methodNameConstant && callInfo.methodInfo.params.map(_.clazz) == $paramTypes
            }

            callInfo.map { callInfo =>
              Call[$reqTpe, $resTpe]($invokerName.$invokeFn(callInfo.asInstanceOf[CallInfo[$reqTpe, $resTpe]], _, ..${paramNames.flatten}))
            }.getOrElse(throw new IllegalAccessException("Couldn't find descriptor for method: " + $methodNameConstant))
          """

        //c.info(c.macroApplication.pos, res.toString(), force = true)
        res
      }


      decl match {
        case m: MethodSymbol if !m.isAccessor =>
          //c.info(c.macroApplication.pos, s"${m.name}: ${m.isMethod}, ${m.isVal}, ${m.isAccessor}", force = true)
          val paramDefs = m.paramLists.map { paramList =>
            paramList.map { param =>
              val name = param.name.toTermName
              val tpe = param.typeSignature.resultType
              q"val $name: $tpe"
            }
          }

          val paramNames = m.paramLists.map { paramList =>
            paramList.map { param =>
              param.name.toTermName
            }
          }

          val methodName = m.name.toTermName

          val paramTypes = m.paramLists.flatMap { paramList =>
            paramList.map { param =>
              val tpe = param.typeSignature.resultType
              //val paramNameLiteral = Literal(Constant(param.name.toString))
              q"""classOf[$tpe]"""
            }
          }

          if (m.isProtected) q"override protected def $methodName(...$paramDefs): $returnType = ${impl(methodName, paramNames, paramTypes)}"
          else q"override def $methodName(...$paramDefs): $returnType = ${impl(methodName, paramNames, paramTypes)}"

        //        case m: MethodSymbol if m.isAccessor =>
        //          val methodName = m.name.toTermName
        //
        //          if (m.isProtected) q"override protected val $methodName: $returnType = ${impl(Nil)}"
        //          else q"override val $methodName: $returnType = ${impl(Nil)}"

        case e => c.abort(c.macroApplication.pos, s"Can't stub this: ${e.name}")
      }
    }

    //c.info(c.macroApplication.pos, invoker.toString(), force = true)

    val t = c.inferImplicitValue(typeOf[Invoker], true)

    if(t.isEmpty) c.abort(c.macroApplication.pos, "Couldn't find implicit value for Invoker")

    val f =
      q"""
          import com.chromy.microservice._
          val $invokerName: $invokerType = $t
          new $serviceTP {
            import scala.concurrent.Future
            ..$s
          }"""
    c.Expr[S](f)
  }


  def apply[S <: Service]: ServiceFactory[S] = macro serviceFactory_impl[S]

  def serviceFactory_impl[S <: Service : c.WeakTypeTag](c: blackbox.Context): c.Expr[ServiceFactory[S]] = {
    import c.universe._
    val serviceTP = weakTypeTag[S]

    val invokerName = TermName("invoker")
    val invokerType = TypeName("Invoker")
    val invokeFn = TermName("invoke")

    val qt = c.typecheck(tq"Call[_, _]").tpe

    val s: Iterable[Tree] = serviceTP.tpe.decls.filter {
      case e if e.typeSignature.finalResultType.typeSymbol == qt.typeSymbol => true
      case _ => false
    }.map { decl =>
      val returnType = decl.typeSignature.finalResultType
      val reqTpe = returnType.typeArgs(0)
      val resTpe = returnType.typeArgs(1)

      def impl(methodName: TermName, paramNames: List[List[TermName]], paramTypes: List[Tree]) = {
        val methodNameConstant = Literal(Constant(methodName.toString))
        val descriptorTermName = TermName("descriptor")

        val res =
          q"""
            val callInfo = $descriptorTermName.calls.find { callInfo =>
             callInfo.methodInfo.method.getName == $methodNameConstant && callInfo.methodInfo.params.map(_.clazz) == $paramTypes
            }

            callInfo.map { callInfo =>
              Call[$reqTpe, $resTpe]($invokerName.$invokeFn(callInfo.asInstanceOf[CallInfo[$reqTpe, $resTpe]], _, ..${paramNames.flatten}))
            }.getOrElse(throw new IllegalAccessException("Couldn't find descriptor for method: " + $methodNameConstant))
          """

        //c.info(c.macroApplication.pos, res.toString(), force = true)
        res
      }


      decl match {
        case m: MethodSymbol if !m.isAccessor =>
          //c.info(c.macroApplication.pos, s"${m.name}: ${m.isMethod}, ${m.isVal}, ${m.isAccessor}", force = true)
          val paramDefs = m.paramLists.map { paramList =>
            paramList.map { param =>
              val name = param.name.toTermName
              val tpe = param.typeSignature.resultType
              q"val $name: $tpe"
            }
          }

          val paramNames = m.paramLists.map { paramList =>
            paramList.map { param =>
              param.name.toTermName
            }
          }

          val methodName = m.name.toTermName

          val paramTypes = m.paramLists.flatMap { paramList =>
            paramList.map { param =>
              val tpe = param.typeSignature.resultType
              //val paramNameLiteral = Literal(Constant(param.name.toString))
              q"""classOf[$tpe]"""
            }
          }

          if (m.isProtected) q"override protected def $methodName(...$paramDefs): $returnType = ${impl(methodName, paramNames, paramTypes)}"
          else q"override def $methodName(...$paramDefs): $returnType = ${impl(methodName, paramNames, paramTypes)}"

        //        case m: MethodSymbol if m.isAccessor =>
        //          val methodName = m.name.toTermName
        //
        //          if (m.isProtected) q"override protected val $methodName: $returnType = ${impl(Nil)}"
        //          else q"override val $methodName: $returnType = ${impl(Nil)}"

        case e => c.abort(c.macroApplication.pos, s"Can't stub this: ${e.name}")
      }
    }

    val f =
      q"""
          import com.chromy.microservice._
          new ServiceFactory[$serviceTP]{
         import scala.concurrent.Future
         def apply($invokerName: $invokerType): $serviceTP = new $serviceTP{
            ..$s
          }
         }"""
    //c.info(c.macroApplication.pos, s"$f", force = true)
    c.Expr[ServiceFactory[S]](f)
  }
}