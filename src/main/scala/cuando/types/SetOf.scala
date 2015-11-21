package cuando.types

import play.api.libs.json.JsSuccess
import play.api.libs.json.Reads
import play.api.libs.json.JsError
import play.api.libs.json.JsArray
import play.api.libs.json.JsResult
import play.api.libs.json.JsValue
import play.api.libs.json.Writes
import play.api.libs.json.JsObject
import play.api.libs.json.Json

case class SetOf(keyField: String, records: Map[KeyFieldVal, Record]) extends SetOfBase

object SetOf {
  implicit def soReads(keyField: String, path: Path, schema: Schema): Reads[SetOf] = new Reads[SetOf] {
    def reads(json: JsValue): JsResult[SetOf] = json match {
      case JsArray(arr) =>
        val records = arr.map { elem =>
          val rec = elem.as[Record](Record.recReads(keyField, path, schema))
          (rec.keyFieldVal -> rec)
        }.toMap
        JsSuccess(SetOf(keyField, records))
      case _ => JsError("error.record.expected.JsArray")
    }
  }

  implicit val soWrites: Writes[SetOf] = new Writes[SetOf] {
    def writes(s: SetOf): JsArray = {
      JsArray(s.records.map { case (key, rcd) => Json.toJson(rcd) }.toSeq)
    }
  }
}
