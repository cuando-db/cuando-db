package cuando

import play.api.libs.json.JsValue
import play.api.libs.json.JsSuccess
import play.api.libs.json.Reads
import play.api.libs.json.JsObject
import play.api.libs.json.JsResult
import play.api.libs.json.JsString
import play.api.libs.json.Writes
import play.api.libs.json.Json
package object types {

  trait SetOfBase
  type RecordContent = Map[String, SetOfBase]
  implicit def rcReads(keyField: String, path: Path, schema: Schema): Reads[RecordContent] = new Reads[RecordContent] {
    def reads(jsContent: JsValue): JsResult[RecordContent] = {
      jsContent match {
        case jsContent: JsObject =>
          JsSuccess(scala.collection.immutable.Map() ++
            jsContent.value.withFilter { case (field, value) => (field != keyField) }.map {
              case (field, value) =>
                val fieldPath = appendToPath(path, field)
                schema.get(fieldPath) match {
                  case Some(SetOfType(keyField)) =>
                    val s1: SetOfBase = value.as[SetOf](SetOf.soReads(keyField, fieldPath, schema))
                    (field -> s1)
                  case Some(SetOfPrefType()) =>
                    val s1: SetOfBase = value.as[SetOfPref]
                    (field -> s1)
                  case None =>
                    throw new IllegalArgumentException("path %s not found in schema".format(fieldPath))
                }
            })
        case _ => throw new IllegalArgumentException("RecordContent must be a JSON object")
      }
    }
  }

  implicit val rcWrites: Writes[RecordContent] = new Writes[RecordContent] {
    def writes(rc: RecordContent): JsValue = {
      JsObject(rc.map {
        case (k, v) => {
          v match {
            case v: SetOf     => (k -> Json.toJson(v))
            case v: SetOfPref => (k -> Json.toJson(v))
          }
        }
      }.toSeq)
    }
  }

  type KeyFieldVal = JsValue
  type PrefRecordContent = JsValue

  sealed trait SubtreeType
  case class SetOfType(keyField: String) extends SubtreeType
  case class SetOfPrefType() extends SubtreeType

  type Path = String
  def appendToPath(path: Path, field: String): Path = path + field + "/"

  type Schema = Map[Path, SubtreeType]
  implicit val sReads: Reads[Schema] = new Reads[Schema] {
    def reads(json: JsValue): JsResult[Schema] = {
      val pathDescMap = scala.collection.immutable.Map() ++ json.as[JsObject].value.map {
        case (path, desc) =>
          val typeName = (desc \ "type")
          typeName match {
            case JsString("setOf") =>
              val keyField = (desc \ "keyField").as[String]
              (path -> SetOfType(keyField))
            case JsString("setOfPref") =>
              (path -> SetOfPrefType())
            case _ => throw new IllegalArgumentException("schema type %s unknown".format(typeName))
          }
        case _ => throw new IllegalArgumentException("unable to parse record in schema")
      }
      JsSuccess(pathDescMap)
    }
  }

  implicit val sWrites: Writes[Schema] = new Writes[Schema] {
    def writes(s: Schema): JsObject = {
      val tmp = s.map {
        case (path, type_) => {
          type_ match {
            case SetOfType(keyField) => (path -> Json.obj("type" -> "setOf", "keyField" -> keyField))
            case SetOfPrefType()     => (path -> Json.obj("type" -> "setOfPref"))
          }
        }
      }
      JsObject(tmp.toSeq)
    }
  }
}