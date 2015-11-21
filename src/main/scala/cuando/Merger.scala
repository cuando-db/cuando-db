package cuando

import cuando.prefs.PreferenceRules
import cuando.time.TemporalContext
import cuando.time.TemporalDimensions
import cuando.time.TemporalRecord
import cuando.types._

case class Merger(schema: Schema, dims: TemporalDimensions, prefs: PreferenceRules) {

  def mergeSetOfs(s1: SetOf, s2: SetOf, path: Path): SetOf = {
    require(s1.keyField == s2.keyField)
    val mergedRecords = s2.records ++ s1.records.map {
      case (keyFieldVal, rec1) =>
        s2.records.get(keyFieldVal) match {
          case Some(rec2) =>
            val mergedRec = mergeRecords(rec1, rec2, path)
            (keyFieldVal -> mergedRec)
          case None => (keyFieldVal -> rec1)
        }
    }
    SetOf(s1.keyField, mergedRecords)
  }

  def mergeRecords(r1: Record, r2: Record, path: Path): Record = {
    require(r1.keyEquals(r2))
    val merged = r2.content ++ r1.content.map {
      case (field1, s1: SetOf) =>
        r2.content.get(field1) match {
          case Some(val2) => val2 match {
            case s2: SetOf =>
              val setOfPath = appendToPath(path, field1)
              (field1 -> mergeSetOfs(s1, s2, setOfPath))
          }
          case None => (field1 -> s1)
        }
      case (field1, s1: SetOfPref) =>
        r2.content.get(field1) match {
          case Some(val2) => val2 match {
            case s2: SetOfPref =>
              val setOfPrefPath = appendToPath(path, field1)
              (field1 -> mergeSetOfPrefs(s1, s2, setOfPrefPath))
          }
          case None => (field1 -> s1)
        }
    }
    Record(r1.keyField, r1.keyFieldVal, merged)
  }

  def mergeSetOfPrefs(s1: SetOfPref, s2: SetOfPref, path: Path): SetOfPref = {
    val s1recs = scala.collection.mutable.Map() ++ s1.records
    val s2recs = scala.collection.mutable.Map() ++ s2.records
    s1recs.foreach {
      case (content1, rec1) =>
        s2recs.foreach {
          case (content2, rec2) =>
            val (resolvedRec1, resolvedRec2) = resolvePrefRecords(rec1, rec2, path)
            s1recs(content1) = resolvedRec1
            s2recs(content2) = resolvedRec2
        }
    }
    val s1resolved = SetOfPref(s1recs.toMap)
    val s2resolved = SetOfPref(s2recs.toMap)
    s1resolved.coalesceWith(s2resolved)
  }

  def resolvePrefRecords(r1: PrefRecord, r2: PrefRecord, path: Path): (PrefRecord, PrefRecord) = {
    val (c1resolved, c2resolved) = resolveTemporalContexts(r1.context, r2.context, path)
    (PrefRecord(r1.content, c1resolved), PrefRecord(r2.content, c2resolved))
  }

  def resolveTemporalContexts(c1: TemporalContext, c2: TemporalContext, path: Path): (TemporalContext, TemporalContext) =
    {
      val xs = scala.collection.mutable.Queue[TemporalRecord]()
      val xsDone = scala.collection.mutable.Queue[TemporalRecord]()
      val ys = scala.collection.mutable.Queue[TemporalRecord]()
      val ysForNextLoop = scala.collection.mutable.Queue[TemporalRecord]()
      var xLoses = false

      xs ++= c1.tRecs
      ys ++= c2.tRecs

      while (xs.isEmpty == false) {
        val x = xs.dequeue
        xLoses = false
        while (ys.isEmpty == false) {
          val y = ys.dequeue
          if (y.overlaps(x, dims)) {
            prefs.eval(path, x, y) match {
              // prefer x
              case Some(true) =>
                val yReplacements = y.difference(x, dims)
                ys ++= yReplacements
              // prefer neither (no preference rules found)
              case None =>
                ysForNextLoop.enqueue(y)
              // prefer y
              case Some(false) =>
                xLoses = true
                val xReplacements = x.difference(y, dims)
                xs ++= xReplacements
                ysForNextLoop.enqueue(y)
                ysForNextLoop ++= ys
                ys.clear
            }
          } else {
            ysForNextLoop.enqueue(y)
          }
        }
        if (xLoses == false) xsDone.enqueue(x)
        ys ++= ysForNextLoop
        ysForNextLoop.clear
      }

      (TemporalContext(xsDone.toSet), TemporalContext(ys.toSet))
    }
}