import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import scala.util.Try

/**
 * Created by cry on 2015.01.19..
 */
object BestBefore extends App{

  val VeryEarly = new Date(1700,1,1)

  def earliestLegal(input: String) : String = {

    val parts = input.split('/')
    val possibleDates = for {
      d <- parts
      m <- parts if(d != m)
      y <- parts if(d != y && m != y)
    } yield {
      val year = y.length match{
        case 1 => "200" + y
        case 2 => "20" + y
        case 3 => "2" + y
        case _ => y
      }
      s"$year/$m/$d"
    }

    val validDates = possibleDates.map { currDate =>
      val df = new SimpleDateFormat("yyyy/MM/dd")
      df.setLenient(false)
      Try{Some(df.parse(currDate))}.getOrElse(None)
    }

    val earliest = (validDates.filter { _.isDefined} map {_.get}).foldLeft(VeryEarly) { (accu, act: Date) =>
      if(act.before(accu)) act
      else accu
    }

    if(earliest.equals(VeryEarly))
      s"$input is illegal"
    else {
      val df = new SimpleDateFormat("yyyy-MM-dd")
      df.format(earliest)
    }
  }

  assert(earliestLegal("02/4/67") == "2067-02-04")
  assert(earliestLegal("31/9/73") == "31/9/73 is illegal")
  println("Success")

}
