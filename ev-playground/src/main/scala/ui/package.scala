/**
  * Created by Gábor on 2016.09.30..
  */
package object ui {
  trait Command

  sealed trait Event

  trait PersistentEvent extends Event
  trait SimpleEvent extends Event
}
