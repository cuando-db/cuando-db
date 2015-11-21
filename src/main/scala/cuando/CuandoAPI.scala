package cuando

import scala.annotation.implicitNotFound

import com.mongodb.casbah.Imports.DBObject
import com.mongodb.casbah.Imports.MongoClient
import com.mongodb.casbah.Imports.MongoDBObject
import com.mongodb.casbah.Imports.wrapDBObj
import com.mongodb.casbah.MongoCollection
import com.mongodb.casbah.map_reduce.MapReduceStandardOutput
import com.mongodb.util.JSON

import cuando.prefs.PreferenceRules
import cuando.time.TemporalDimensions
import cuando.types.Path
import cuando.types.Record
import cuando.types.Schema
import cuando.types.SetOfPrefType
import cuando.types.SetOfType
import cuando.types.sReads
import cuando.types.sWrites
import play.api.libs.json.JsArray
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper

case class CuandoAPI(mongoClient: MongoClient) {

  val cuandoDB = mongoClient("cuando")
  val metadata = cuandoDB("metadata")
  /*
	 * Create collection 'archiveName'
	 * If collection 'metadata'doesn't exist, create it
	 * To 'metadata' add 'archiveName' document with schema, dims, prefs subdocuments
	 */
  def init(archiveName: String, schema: Schema, dims: TemporalDimensions, prefs: PreferenceRules): JsValue = {
    val query = MongoDBObject("archiveName" -> archiveName)
    metadata.remove(query)
    cuandoDB(archiveName).drop()

    val archive = cuandoDB(archiveName) // create collection
    createIndexes(archive, schema)

    // must serialize schema and prefs manually
    val prefsJson = JSON.parse(Json.toJson(prefs).toString)
    val schemaJson = JSON.parse(Json.toJson(schema).toString)
    val metadataRecord = MongoDBObject(
      "archiveName" -> archiveName,
      "schema" -> schemaJson,
      "dims" -> dims,
      "prefs" -> prefsJson)
    metadata.insert(metadataRecord)

    Json.obj("success" -> true,
      "metadata" -> com.mongodb.util.JSON.serialize(metadataRecord))
  }

  private def createIndexes(archive: MongoCollection, schema: Schema) {
    val mongoPathsToIndex = schema.map {
      case (path, SetOfType(kf))   => mongoDBPath(path + kf + "/")
      case (path, SetOfPrefType()) => mongoDBPath(path)
    }
    mongoPathsToIndex.foreach { path => archive.createIndex(MongoDBObject(path -> 1)) }
  }

  /*
   * To merge, we need initialized database.
   * We first retrieve the type of "/" from the Schema.
   * We'll use this type when we parse the JSON input
   */
  def merge(archiveName: String, doc: JsValue): JsValue = {
    val query = MongoDBObject("archiveName" -> archiveName)
    val result = metadata.findOne(query)
    result match {
      case Some(archiveMetadata) => {
        (archiveMetadata.getAs[DBObject]("schema"),
          archiveMetadata.getAs[DBObject]("dims"),
          archiveMetadata.getAs[DBObject]("prefs")) match {
            case (Some(schemaJson), Some(dimsJson), Some(prefsJson)) => {
              val schema = Json.parse(schemaJson.toString).as[Schema]
              val dims = Json.parse(dimsJson.toString).as[TemporalDimensions]
              val prefs = Json.parse(prefsJson.toString).as[PreferenceRules]

              val merger = Merger(schema, dims, prefs)
              val archive = cuandoDB(archiveName)
              val rootPath: Path = "/"
              schema.get(rootPath) match {
                case Some(root: SetOfType) => {
                  val newRecord = doc.as[Record](Record.recReads(root.keyField, rootPath, schema))

                  val query = MongoDBObject("content." + root.keyField -> JSON.parse(newRecord.keyFieldVal.toString))
                  archive.findOne(query) match {
                    case Some(oldDoc) => {
                      val oldDocId = oldDoc.get("_id")
                      oldDoc.removeField("_id")

                      val oldRecord = Json.parse(oldDoc.toString).as[Record](Record.recReads(root.keyField, rootPath, schema))
                      val mergedRecord = merger.mergeRecords(oldRecord, newRecord, rootPath)

                      val mergedDoc = JSON.parse(Json.toJson(mergedRecord).toString).asInstanceOf[DBObject]
                      val updateQuery = MongoDBObject("_id" -> oldDocId)
                      val result = archive.update(updateQuery, mergedDoc)
                      Json.obj("success" -> true, "status" -> "merged")
                    }
                    case None =>
                      val newDoc = JSON.parse(Json.toJson(newRecord).toString).asInstanceOf[DBObject]
                      val result = archive.insert(newDoc)
                      Json.obj("success" -> true, "status" -> "inserted")
                  }

                }
                case Some(_) => {
                  val error = "Schema root has invalid type"
                  Json.obj("success" -> false, "error" -> error)
                }
                case None => {
                  val error = "Schema root is missing"
                  Json.obj("success" -> false, "error" -> error)
                }
              }

            }
            case _ => {
              val error = "Archive %s metadata is incomplete: %s".format(
                archiveName, archiveMetadata.toString())
              Json.obj("success" -> false, "error" -> error)
            }
          }
      }
      case None => throw new IllegalArgumentException(
        "Archive %s metadata not found".format(archiveName))
    }
  }

  def query(archiveName: String, sc: SelectClause, pc: Option[ProjectClause] = None, tc: Option[TimeSliceClause] = None): JsValue = {
    val archive = cuandoDB(archiveName)
    tc match {
      case tc: Some[TimeSliceClause] => {
        val cursor = timesliceQuery(archive, sc, pc, tc)
        val docs = cursor.toList
        JsArray(docs.map { doc =>
          val value = doc.get("value").asInstanceOf[DBObject]
          value.removeField("_id")
          Json.parse(value.toString)
        })
      }
      case None => {
        val cursor = simpleQuery(archive, sc, pc)
        val docs = cursor.toList
        JsArray(docs.map { doc =>
          doc.removeField("_id")
          Json.parse(doc.toString)
        })
      }
    }
  }

  private def simpleQuery(archive: MongoCollection, sc: SelectClause, pc: Option[ProjectClause]) = {
    pc match {
      case Some(pc) => archive.find(sc, pc)
      case None     => archive.find(sc)
    }
  }

  private def timesliceQuery(archive: MongoCollection, sc: SelectClause, pc: Option[ProjectClause], tc: Option[TimeSliceClause]) = {
    val mrOutputCollection = "mr_out"
    val map = "function() { emit(this._id, filter(this, timeIntervalFilter)); }"
    val reduce = "function(k,v) { return null; }"
    val output = MapReduceStandardOutput(mrOutputCollection)
    val query = Some(SelectClause.scToMongoQueryDoc(sc))
    val jsScope = tc match {
      case Some(tc) => Some(TimeSliceClause.tcToMongoDoc(tc))
      case None     => None
    }

    cuandoDB.eval("db.loadServerScripts()")
    var res = archive.mapReduce(map, reduce, output, query, None, None, None, jsScope)
    pc match {
      case Some(pc) => {
        pc.prefix = Some("value.")
        cuandoDB(mrOutputCollection).find(MongoDBObject.empty, pc)
      }
      case None => cuandoDB(mrOutputCollection).find(MongoDBObject.empty)
    }
  }
}
