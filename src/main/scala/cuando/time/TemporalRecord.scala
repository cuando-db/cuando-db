package cuando.time

import play.api.libs.json.JsResult
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import cuando.prefs.Pred

case class TemporalRecord(dimIntMap: Map[TemporalDimension, TemporalInterval]) {
  def overlaps(that: TemporalRecord, ds: TemporalDimensions): Boolean =
    this.dimIntMap.forall {
      case (dimension, interval) =>
        that.dimIntMap(dimension).intersects(interval)
    }

  def difference(that: TemporalRecord, dimensions: TemporalDimensions): Set[TemporalRecord] = {

    def diff(winner: Map[TemporalDimension, TemporalInterval],
             loser: Map[TemporalDimension, TemporalInterval],
             dimensions: TemporalDimensions): Set[Map[TemporalDimension, TemporalInterval]] =
      dimensions.isEmpty match {
        case true => Set()
        case false =>
          val d = dimensions.head
          val ds = dimensions.tail
          loser(d).intersect(winner(d)) match {
            case None => diff(winner, loser, ds)
            case Some(interwrtd) =>
              val diffwrtd = loser(d).difference(winner(d))
              val resolved = diffwrtd.map { interval => loser ++ Map(d -> interval) }
              resolved union diff(winner, loser ++ Map(d -> interwrtd), ds)
          }
      }

    diff(that.dimIntMap, this.dimIntMap, dimensions).map { dimIntMap => TemporalRecord(dimIntMap) }.toSet
  }

  override def toString = {
    val sb = new StringBuilder()
    var first = true
    for ((d, interval) <- dimIntMap) {
      if (first) {
        sb append d
        sb append ":"
        sb append tiToString(interval)
        first = false
      } else {
        sb append ","

        sb append d
        sb append ":"
        sb append tiToString(interval)
      }
    }
    sb.toString
  }
}

object TemporalRecord {
  implicit val trReads: Reads[TemporalRecord] = new Reads[TemporalRecord] {
    def reads(json: JsValue): JsResult[TemporalRecord] =
      JsSuccess(TemporalRecord(json.as[Map[TemporalDimension, TemporalInterval]]))
  }
  implicit val trWrites: Writes[TemporalRecord] = new Writes[TemporalRecord] {
    def writes(tr: TemporalRecord): JsValue =
      Json.toJson(tr.dimIntMap)
  }

}