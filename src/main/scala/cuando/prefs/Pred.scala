package cuando.prefs
import cuando.time._
import play.api.libs.json.JsValue
import play.api.libs.json.Reads
import play.api.libs.json.JsResult
import play.api.libs.json.JsSuccess
import play.api.libs.json.Json
import play.api.libs.json.Writes

case class Pred(v1: PredArg, v2: PredArg, op: PredOp) {
  def eval(r1: TemporalRecord, r2: TemporalRecord): Boolean =
    op.eval(v1.get(r1), v2.get(r2))
}

object Pred {
  implicit val predReads: Reads[Pred] = Json.reads[Pred]
  implicit val predWrites: Writes[Pred] = Json.writes[Pred]
}