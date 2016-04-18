package concept

/**
 * Created by cry on 2016.04.13..
 */
sealed trait Validation[+F, +S] {
  def flatMap[FB >: F, SB >: S](f: Seq[S] => Validation[FB, SB]): Validation[FB, SB] = this match {
    case SuccessfulValidation(events) => f(events) match {
        case SuccessfulValidation(newEvents) => SuccessfulValidation(events ++ newEvents)
        case e@FailedValidation(_) => e
    }
    case e@FailedValidation(_) => e
  }

  def map[SB >: S](f: Seq[S] => Seq[SB]): Validation[F, SB] = this match {
    case SuccessfulValidation(events) =>  
      SuccessfulValidation(f(events))
    case e@FailedValidation(_) => e
  }
  
  def isFailed = !isSuccess
  def isSuccess = this match {
    case SuccessfulValidation(_) => true
    case FailedValidation(_) => false
  }
  
}

case class SuccessfulValidation[+S](events: Seq[S]) extends Validation[Nothing, S]
case class FailedValidation[+F](failure: F) extends Validation[F, Nothing]
