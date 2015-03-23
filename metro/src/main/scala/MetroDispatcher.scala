package metro
/**
 * Created by cry on 2015.02.15..
 */

import akka.actor.FSM

import scala.language.postfixOps

//class MetroDispatcher(tunnel: SubwayTunnel) extends FSM[List[TubeSectionState], Unit] {
//
//  val initState = tunnel.sections.map { s => TubeSectionState(s, false, false, false)}
//
//  startWith(initState, _)
//
//  onTransition {
//    case _ =>
//  }
//
//
//    //  def receive = {
////    case Start =>
////      val newTrain = context.actorOf(MetroTrain.props(tunnel, tunnel.FirstPosition))
////      newTrain ! Start
////      context.system.scheduler.schedule(0 milliseconds, 1 seconds, newTrain, Tick)
////    case _ =>
////  }
//
//  initialize()
//}
