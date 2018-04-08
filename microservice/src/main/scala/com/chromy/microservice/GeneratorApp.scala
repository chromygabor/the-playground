package com.chromy.microservice

import akka.http.scaladsl.model.HttpMethods

object GeneratorApp extends App {

  for {i <- 0 to 22} {

    //implicit def func0ToMI[Req, Res](f: => Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]
    //implicit def func2ToMI[Req, Res, P1, P2](f: (P1, P2) => Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]

    val types = (1 to i).map(i => s"P$i")

    val typeParameters = ("Req" +: "Res" +: types).mkString(", ")
    val functionTypes = (types).mkString("(", ", ", ")")
    val params = for {p <- 1 to i} yield {
      s"p$p: P$p"
    }
    val paramNames = params.mkString(", ")
    val paramNamesWithRequest = ("request: Req" +: params).mkString(", ")
    val paramNamesWithRequestUnderscore = ("_: Req" +: params).mkString(", ")

    val s =
      s"""implicit def func${i}ToMI[$typeParameters](f: ${functionTypes}=> Call[Req, Res]): MethodInfo[Req, Res] = macro of_impl[Req, Res]""".stripMargin
    println(s)
  }


}
