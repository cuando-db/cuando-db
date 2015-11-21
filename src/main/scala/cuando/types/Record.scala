package cuando.types

import play.api.libs.json.JsError
import play.api.libs.json.JsObject
import play.api.libs.json.JsResult
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsValue
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.libs.json.Json

case class Record(keyField: String, keyFieldVal: KeyFieldVal, content: RecordContent) {
  def keyEquals(that: Record): Boolean = {
    (this.keyField == that.keyField) && (this.keyFieldVal == that.keyFieldVal)
  }
}

object Record {
  implicit def recReads(keyField: String, path: Path, schema: Schema): Reads[Record] = new Reads[Record] {
    def reads(json: JsValue): JsResult[Record] = {
      val jsContent = (json \ "content").as[JsObject]

      require(jsContent.keys.contains(keyField))
      val keyFieldVal: KeyFieldVal = jsContent \ keyField
      val content = jsContent.as[RecordContent](rcReads(keyField, path, schema))
      JsSuccess(Record(keyField, keyFieldVal, content))
    }
  }

  implicit val recWrites = new Writes[Record] {
    def writes(rec: Record) = {
     val jsContent = Json.toJson(rec.content).asInstanceOf[JsObject]
     val amendedContent = jsContent + (rec.keyField -> rec.keyFieldVal)
     Json.obj("content" -> amendedContent)
    }
  }
}