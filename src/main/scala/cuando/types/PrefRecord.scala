package cuando.types

import play.api.libs.json.Json
import cuando.time._


case class PrefRecord(content: PrefRecordContent, context: TemporalContext) {
  def coalesceWith(that: PrefRecord): PrefRecord = {
    require(this.content == that.content)
    PrefRecord(this.content, this.context union that.context)
  }
}

object PrefRecord {
  implicit val prReads = Json.reads[PrefRecord]
  implicit val prWrites = Json.writes[PrefRecord]
}