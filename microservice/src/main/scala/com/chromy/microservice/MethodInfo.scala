package com.chromy.microservice

import java.lang.reflect.Method

import scala.collection.immutable.Seq
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

class MethodInfo[Req, Res](clazz: Class[_], val method: Method, val params: Seq[ParamInfo[_]])

object MethodInfo {

  def apply[Req, Res](clazz: Class[_], methodName: String, paramInfo: Seq[ParamInfo[_]]): MethodInfo[Req, Res] = {
    val method = clazz.getMethod(methodName, paramInfo.map(_.clazz): _*)
    new MethodInfo[Req, Res](clazz, method, paramInfo)
  }

  implicit def func0ToMI[Req, Res](f: => Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func0ToMI[Req, Res](f: ()=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func1ToMI[Req, Res, P1](f: (P1)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func2ToMI[Req, Res, P1, P2](f: (P1, P2)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func3ToMI[Req, Res, P1, P2, P3](f: (P1, P2, P3)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func4ToMI[Req, Res, P1, P2, P3, P4](f: (P1, P2, P3, P4)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func5ToMI[Req, Res, P1, P2, P3, P4, P5](f: (P1, P2, P3, P4, P5)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func6ToMI[Req, Res, P1, P2, P3, P4, P5, P6](f: (P1, P2, P3, P4, P5, P6)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func7ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7](f: (P1, P2, P3, P4, P5, P6, P7)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func8ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8](f: (P1, P2, P3, P4, P5, P6, P7, P8)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func9ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func10ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func11ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func12ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func13ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func14ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func15ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func16ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func17ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func18ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func19ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func20ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func21ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
  implicit def func22ToMI[Req, Res, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22](f: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22)=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]

  def of_impl[Req: c.WeakTypeTag, Res: c.WeakTypeTag](c: blackbox.Context)(f: c.Tree): c.Tree = {
    import c.universe._

    val tpReq = weakTypeTag[Req]
    val tpRes = weakTypeTag[Res]

    //c.info(c.enclosingPosition, s"$f", false)

    val (thisType, methodName, params) = f match {
      // Functions references with parameter lists
      case Block((_, Function(args, Apply(Select(This(tt), TermName(tn)), _)))) =>
        //c.info(c.enclosingPosition, s"Functions references with parameter lists", false)
        (tt, tn, args)
      // Functions references with no parameter lists
      case Function(_, Select(This(tt), TermName(tn))) =>
        //c.info(c.enclosingPosition, s"Functions references with no parameter lists", false)
        (tt, tn, Nil)
      // Pass in the result of a function with a parameter list
      case Apply(Select(This(tt), TermName(tn)), args) =>
        //c.info(c.enclosingPosition, s"Pass in the result of a function with a parameter list, $args", false)
        (tt, tn, Nil)
      // Pass in the result of a function with no parameter list
      case Select(This(tt), TermName(tn)) =>
        //c.info(c.enclosingPosition, s"Pass in the result of a function with no parameter list", false)
        (tt, tn, Nil)
      case other =>
        c.abort(c.enclosingPosition, s"must only be invoked with a reference to a function on this, for example, (this.someFunction _)")
    }


    val methodNameLiteral = Literal(Constant(methodName))
    val pt = params.map {
      case q"$mod val $name: $tp = $v" =>
        val nameLiteral = Literal(Constant(name.toString()))
        q"com.chromy.microservice.ParamInfo[$tp]($nameLiteral, _root_.scala.Predef.classOf[$tp], implicitly[com.chromy.microservice.ParamSerializer[$tp]])"
      case e =>
        c.abort(c.enclosingPosition, s"Couldn't find out type of parameter: $params")
    }

    //c.info(c.enclosingPosition, s"$methodName, $thisType, $pt", false)

    val thisClassExpr = if (thisType.toString.isEmpty) {
      // If the type is empty, it means the reference to this is unqualified, eg:
      // namedCall(this.someServiceCall _)
      q"this.getClass"
    } else {
      // Otherwise, it's a qualified reference, and we should use that type, eg:
      // namedCall(MyService.this.someServiceCall _)
      // This also happens to be what the type checker will infer when you don't explicitly refer to this, eg:
      // namedCall(someServiceCall _)
      q"_root_.scala.Predef.classOf[$thisType]"
    }

    val res =
      q"""
         com.chromy.microservice.MethodInfo.apply[$tpReq, $tpRes]($thisClassExpr, $methodNameLiteral, $pt)"""
    //c.info(c.macroApplication.pos, res.toString(), force = true)

    res
  }
}