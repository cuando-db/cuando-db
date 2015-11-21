package cuando

import play.api.libs.json.JsSuccess
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.libs.json.JsResult
import play.api.libs.json.JsValue
import continuum.Discrete
import continuum.Interval
import org.joda.time.DateTime
import play.api.libs.json.Json

package object time {
  val EARLIEST = DateTime.parse("1970-01-01")
  val NOW = DateTime.parse("2999-12-31")
  val DATETIME_PATTERN = "yyyy-MM-dd'T'H:m:s'Z'" // see http://www.joda.org/joda-time/key_format.html
  val dtFormatter = DateTimeFormat.forPattern(DATETIME_PATTERN)
    
  type TemporalDimension = String
  type TemporalDimensions = Set[TemporalDimension]

  type TemporalInterval = Interval[DateTime]
  def TemporalInterval(start: DateTime, finish: DateTime) = Interval.closedOpen(start, finish)
  implicit class TemprovalIntervalImproved(val ti: TemporalInterval) {
    def start: DateTime = ti.normalize(dtDiscrete) match {
      case (Some(start), _) => start
      case (_, _)           => throw new IllegalArgumentException("temporal interval has no start value")
    }
    def finish: DateTime = ti.normalize(dtDiscrete) match {
      case (_, Some(finish)) => finish
      case (_, _)            => throw new IllegalArgumentException("temporal interval has no finish value")
    }
  }

  implicit val dateTimeOrdering: Ordering[DateTime] =
    Ordering.fromLessThan(_ isBefore _)

  implicit val dtDiscrete: Discrete[DateTime] = new Discrete[DateTime] {
    override def next(dt: DateTime): Option[DateTime] = Some(dt)
  }

  implicit def tiToString(ti: TemporalInterval): String = {
    ti.normalize(dtDiscrete) match {
      case (Some(start), Some(finish)) =>
        "[" + dtFormatter.print(start) + ", " +
          ((finish == NOW) match {
            case true  => "now"
            case false => dtFormatter.print(finish)
          }) + ")"
      case _ => throw new IllegalArgumentException("invalid interval")
    }
  }

  // will override DefaultJodaDateReads (see Reads.scala)
  implicit val tiJodaDateReads: Reads[DateTime] = (Reads.jodaDateReads(DATETIME_PATTERN))
  implicit val tiJodaDateWrites: Writes[DateTime] = (Writes.jodaDateWrites(DATETIME_PATTERN))

  implicit val tiReads: Reads[TemporalInterval] = new Reads[TemporalInterval] {
    def reads(json: JsValue): JsResult[TemporalInterval] = {
      val start = (json \ "start").as[DateTime]
      val finishJson = (json \ "finish")
      val finish = finishJson.as[String] match {
        case "now" => NOW
        case _     => finishJson.as[DateTime]
      }
      JsSuccess(Interval.closedOpen(start, finish))
    }
  }
  implicit val tiWrites: Writes[TemporalInterval] = new Writes[TemporalInterval] {
    def writes(i: Interval[DateTime]): JsValue = {
      val (start, finish) = i.normalize(dtDiscrete)
      (finish == NOW) match {
        case true  => Json.obj("start" -> start, "finish" -> "now")
        case false => Json.obj("start" -> start, "finish" -> finish)
      }
    }
  }

}