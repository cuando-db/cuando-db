package cuando.prefs

import cuando.time._
import org.joda.time.DateTime
import cuando.time.TemporalDimension
import cuando.time.TemporalRecord
import cuando.time.TemprovalIntervalImproved
import play.api.libs.json.Reads
import play.api.libs.json.Json
import play.api.libs.json.JsValue
import play.api.libs.json.JsResult
import play.api.libs.json.JsString
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsObject
import play.api.libs.json.Writes

abstract class PredArg {
  def get(record: TemporalRecord): DateTime
}
object PredArg {
  implicit val predArgReads: Reads[PredArg] = new Reads[PredArg] {
    def reads(json: JsValue): JsResult[PredArg] = {
      val arg = json match {
        case json: JsObject =>
          (json \ "type").as[String] match {
            case "const" => (json \ "parameters").as[PredConst]
            case "attr"  => (json \ "parameters").as[PredAttr]
          }
        case _ => throw new IllegalArgumentException("Predicate argument must be a JSON object")
      }
      JsSuccess(arg)
    }
  }
  implicit val predArgWrites: Writes[PredArg] = new Writes[PredArg] {
    def writes(predArg: PredArg): JsValue = predArg match {
      case predAttr: PredAttr =>
        Json.obj("type" -> "attr",
          "parameters" -> predAttr.toJson)
      case predConst: PredConst =>
        Json.obj("type" -> "const",
          "parameters" -> predConst.toJson)
    }
  }
}

case class PredConst(dt: DateTime) extends PredArg {
  def get(record: TemporalRecord): DateTime = dt
  def toJson: JsValue = Json.obj(
    "dt" -> Json.toJson(dt))
}
object PredConst {
  implicit val predConstReads: Reads[PredConst] = Json.reads[PredConst]
}

sealed trait Bound
case object Start extends Bound
case object Finish extends Bound
object Bound {
  implicit val boundReads: Reads[Bound] = new Reads[Bound] {
    def reads(json: JsValue): JsResult[Bound] = {
      val bound = json match {
        case JsString("start")  => Start
        case JsString("finish") => Finish
        case _                  => throw new IllegalArgumentException("%s bound undefined".format(json))
      }
      JsSuccess(bound)
    }
  }
  implicit val boundWrites: Writes[Bound] = new Writes[Bound] {
    def writes(bound: Bound): JsString =
      bound match {
        case Start  => JsString("start")
        case Finish => JsString("finish")
      }
  }
}

case class PredAttr(dim: TemporalDimension, bound: Bound) extends PredArg {
  def get(record: TemporalRecord): DateTime = {
    record.dimIntMap.get(dim) match {
      case Some(ti) => bound match {
        case Start  => ti.start
        case Finish => ti.finish
      }
      case None => throw new IllegalArgumentException("temporal interval has no dimension %s".format(dim))
    }
  }
  def toJson: JsValue = Json.obj(
    "dim" -> Json.toJson(dim),
    "bound" -> Json.toJson(bound))
}
object PredAttr {
  implicit val predAttrReads: Reads[PredAttr] = Json.reads[PredAttr]
}