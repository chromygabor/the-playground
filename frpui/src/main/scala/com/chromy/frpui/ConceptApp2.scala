package com.chromy.frpui

import com.chromy.frpui.core.{Model, _}

import scala.concurrent.Future

/**
 * Created by cry on 2015.10.15..
 */
object ConfigService {
  case class Changed(config: Map[String, String] = Map.empty) extends Action
}
trait ConfigService {
  def set(key: String, value: String): Unit
  def config: Map[String, String]
  def subscribe(uid: Uid): Unit
}

case class ConfigServiceImpl(uid: Uid = Uid(), config: Map[String, String] = Map("key1" -> "default1", "key2" -> "default2" )) extends Service[ConfigService, ConfigServiceImpl] {

  case class ConfigSet(key: String, value: String) extends Action
  
  override def handle(implicit context: Context): Updater[ConfigServiceImpl] = Updater {
    case Init =>
      println(s"Service initialized")
      this
    case ConfigSet(key: String, value: String) =>
      val newConfig = config.updated(key, value) 
      context.onAction(ConfigService.Changed(newConfig))
      copy(config = newConfig)
  }

  override def api(context: Context): ConfigService = new ConfigService {
    override def set(key: String, value: String): Unit = context.onAction(ConfigSet(key, value))

    override def subscribe(uid: Uid): Unit = context.onAction(Targeted(uid, ConfigService.Changed(config)))

    override lazy val config: Map[String, String] = ConfigServiceImpl.this.config
  }
}

case class MainModel(value: Int = 0, uid: Uid = Uid()) extends Model[MainModel] {
  
  override def handle(implicit context: Context): Updater[MainModel] = Updater {
    case Init =>
      context.getService[ConfigService].subscribe(uid)
      this
    case ConfigService.Changed(config) =>
      println(s"Config was changed we start a future task: $config")
      
      import scala.concurrent.ExecutionContext.Implicits.global

      val service = context.getService[ConfigService]
      
      if(!service.config.contains("Later")) {
        println("Before sleep: " + service.config.get("Later"))
//        Future {
//          println("It is sleeping")
//          Thread.sleep(3000)
//          println("It is done with sleeping")
//          defer { (laterContext, laterModel) =>
//            println("You can see, we changed the config later than we started the Sleep")
//            println("After the sleep" + laterContext.getService[ConfigService].config.get("Later"))
//            laterModel
//          }
//          this
//        }
        context.getService[ConfigService].set("Later", "test")
      }
      this
  }
}

/**
 * Main app
 */
object ConceptApp2 extends App {

  lazy val services = Map[Class[_], ServiceBuilder[_]](
    classOf[ConfigService] -> ServiceBuilder.singleton(ConfigServiceImpl())
  )


  val app = new FrpApp(MainModel(), services)

  val u1 = Uid()
  app.onNext(Init)
  // s.onNext(AddItem(u1))
  // s.onNext(IncrementValue(2, u1))
  // s.onNext(IncrementValue(5, u1))

  val u2 = Uid()
  //  s.onNext(AddValue(u2))
  //  s.onNext(IncrementValue(7, u2))
  //  s.onNext(IncrementValue(9, u2))
  //  s.onNext(IncrementValue(11, u2))

  Thread.sleep(5000)
}
