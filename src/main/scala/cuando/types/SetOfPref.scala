package cuando.types

import play.api.libs.json.JsArray
import play.api.libs.json.JsError
import play.api.libs.json.JsResult
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsValue
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.libs.json.Json

case class SetOfPref(records: Map[PrefRecordContent, PrefRecord]) extends SetOfBase {

  /*
   * Unions TemporalContexts of content-equivalent PrefRecords
   */
  def coalesceWith(that: SetOfPref): SetOfPref = {
    val coalescedPrefRecords = that.records ++ this.records.map {
      case (thisRecContent, thisRec) =>
        that.records.get(thisRecContent) match {
          case Some(thatRec) =>
            val coalescedRecord = thisRec.coalesceWith(thatRec)
            (thisRecContent -> coalescedRecord)
          case None => (thisRecContent -> thisRec)
        }
    }
    SetOfPref(coalescedPrefRecords)
  }
}

object SetOfPref {
  implicit val sopReads: Reads[SetOfPref] = new Reads[SetOfPref] {
    def reads(json: JsValue): JsResult[SetOfPref] = json match {
      case JsArray(arr) =>
        val records = arr.map { elem =>
          val rec = elem.as[PrefRecord]
          val content = rec.content
          (rec.content, rec)
        }.toMap
        JsSuccess(SetOfPref(records))
      case _ => JsError("error.record.expected.JsArray")
    }
  }

  implicit val sopWrites: Writes[SetOfPref] = new Writes[SetOfPref] {
    def writes(s: SetOfPref): JsArray = {
            JsArray(s.records.map { case (key, prefRcd) => Json.toJson(prefRcd) }.toSeq)
    }
  }
}
