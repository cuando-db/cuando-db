package cuando.time

import play.api.libs.json.JsSuccess
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.libs.json.JsResult
import play.api.libs.json.JsValue
import continuum.Interval
import org.joda.time.DateTime
import play.api.libs.json.Json
import play.api.libs.json.JsArray
import play.api.libs.json.JsError

case class TemporalContext(tRecs: Set[TemporalRecord]) {
  def union(that: TemporalContext): TemporalContext = {
    TemporalContext(this.tRecs union that.tRecs)
  }
}

object TemporalContext {
  implicit val tcReads: Reads[TemporalContext] = new Reads[TemporalContext] {
    def reads(json: JsValue): JsResult[TemporalContext] = {
      json match {
        case JsArray(arr) =>
          val tRecs = arr.map { elem => elem.as[TemporalRecord] }.toSet
          JsSuccess(TemporalContext(tRecs))
        case _ => JsError("error.record.expected.JsArray")
      }
    }
  }

  implicit val tcWrites: Writes[TemporalContext] = new Writes[TemporalContext] {
    def writes(tc: TemporalContext): JsArray = {
      JsArray(tc.tRecs.map { temporalRec => Json.toJson(temporalRec) }.toSeq)
    }
  }
}
