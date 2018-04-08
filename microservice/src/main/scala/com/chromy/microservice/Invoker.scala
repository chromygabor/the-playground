package com.chromy.microservice

import scala.concurrent.Future

trait Invoker {
  def invoke[Req, Res](callInfo: CallInfo[Req, Res], request: Req, args: Any*): Future[Res]
}
