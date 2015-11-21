package cuando.prefs

import cuando.time.TemporalRecord
import cuando.types.Path
import play.api.libs.json.JsSuccess
import play.api.libs.json.Reads
import play.api.libs.json.JsResult
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import play.api.libs.json.Writes
import play.api.libs.json.Json

case class PreferenceRules(pathPredsMap: Map[Path, Set[Pred]]) {
  def eval(path: Path, r1: TemporalRecord, r2: TemporalRecord): Option[Boolean] =
    pathPredsMap.get(path) match {
      case Some(setOfPreds) => Some(setOfPreds.forall { pred => pred.eval(r1, r2) })
      case None             => None
    }
}

object PreferenceRules {
  implicit val prefsReads: Reads[PreferenceRules] = new Reads[PreferenceRules] {
    def reads(json: JsValue): JsResult[PreferenceRules] = {
      JsSuccess(PreferenceRules(json.as[Map[Path, Set[Pred]]]))
    }
  }

  implicit val prefsWrites: Writes[PreferenceRules] = new Writes[PreferenceRules] {
    def writes(prefs: PreferenceRules): JsObject =
      JsObject(prefs.pathPredsMap.map {
        case (k, v) => (k -> Json.toJson(v))
      }.toSeq)
  }
}