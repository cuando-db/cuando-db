import play.api.libs.json.JsValue
import cuando.time.TemporalDimension
import org.joda.time.DateTime
import cuando.types.Path
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.util.JSON
import play.api.libs.json.Json
import play.api.libs.json.JsSuccess
import java.text.DateFormat
import play.api.libs.json.Reads
import play.api.libs.json.JsError
import play.api.libs.json.JsUndefined
import play.api.libs.json.JsObject
import play.api.libs.json.JsArray
import play.api.libs.json.JsNull
import play.api.libs.json.JsResult
import play.api.data.validation.ValidationError
import play.api.libs.json.JsString
import play.api.libs.json.JsNumber
import play.api.libs.json.JsBoolean
import play.api.libs.json.JsPath
import org.bson.types.ObjectId
import java.util.Date
import com.mongodb.DBObject
import play.api.libs.json.JsSuccess
import org.joda.time.format.DateTimeFormat
import cuando.time._
import play.api.libs.json.JsNumber

package object cuando {

  case class ProjectClause(paths: List[Path]) {
    var prefix: Option[String] = None
  }
  object ProjectClause {
    // see https://docs.mongodb.org/manual/tutorial/project-fields-from-query-results/
    implicit def pcToMongoProjectionDoc(pc: ProjectClause) = {
      val fields = pc.paths.collect {
        case p => {
          pc.prefix match {
            case Some(prefix) => (new Path(prefix + mongoDBPath(p)) -> 1)
            case None         => (mongoDBPath(p) -> 1)
          }
        }
      }
      MongoDBObject(fields)
    }

    implicit val pcReads: Reads[ProjectClause] = new Reads[ProjectClause] {
      def reads(json: JsValue): JsResult[ProjectClause] = {
        val pc = ProjectClause(json.as[List[Path]])
        JsSuccess(pc)
      }
    }
  }

  case class SelectClause(tuples: Map[Path, JsValue])
  object SelectClause {
    implicit def scToMongoQueryDoc(sc: SelectClause) = {
      val builder = MongoDBObject.newBuilder
      sc.tuples.foreach {
        case (p, v) => {
          v match {
            case z: JsObject => builder += (mongoDBPath(p) -> MongoJson.fromJson(z).get)
            case z: JsString => builder += (mongoDBPath(p) -> z.value)
            case z: JsNumber => builder += (mongoDBPath(p) -> z.value)
            case _           => throw new IllegalArgumentException("Value must be a JSON object, string, or number.")
          }
        }
      }
      builder.result
    }

    implicit val scReads: Reads[SelectClause] = new Reads[SelectClause] {
      def reads(json: JsValue): JsResult[SelectClause] = {
        val sc = SelectClause(json.as[Map[Path, JsValue]])
        JsSuccess(sc)
      }
    }
  }

  case class TimeSliceClause(dim: TemporalDimension, start: DateTime, finish: DateTime)
  object TimeSliceClause {
    implicit def tcToMongoDoc(tc: TimeSliceClause) = {
      MongoDBObject(
        "dimension" -> tc.dim,
        "start" -> tc.start.toString(dtFormatter),
        "finish" -> tc.finish.toString(dtFormatter))
    }

    implicit val tcReads: Reads[TimeSliceClause] = new Reads[TimeSliceClause] {
      def reads(json: JsValue): JsResult[TimeSliceClause] = {
        val finishJson = (json \ "finish")
        val finish = finishJson.as[String] match {
          case "now" => NOW
          case _     => finishJson.as[DateTime]
        }
        val sc = TimeSliceClause((json \ "dimension").as[TemporalDimension], (json \ "start").as[DateTime], finish)
        JsSuccess(sc)
      }
    }
  }

  // from https://gist.github.com/doitian/5555040
  object MongoJson {
    def fromJson(json: JsValue): JsResult[DBObject] = readDBObject.reads(json)

    implicit val readDBObject = new Reads[DBObject] {
      def reads(js: JsValue): JsResult[DBObject] = {
        parsePlainObject(js.asInstanceOf[JsObject], JsPath())
      }

      private def parsePlainObject(obj: JsObject, parent: JsPath): JsResult[DBObject] = {
        parsePlainFields(obj.fields.toList, parent).map(MongoDBObject(_))
      }

      private def parsePlainFields(l: List[(String, JsValue)], parent: JsPath): JsResult[List[(String, Any)]] = {
        l match {
          case Nil => JsSuccess(Nil, parent)
          case head :: tail => cons(
            parse(head._2, (parent \ head._1)).map(head._1 -> _),
            parsePlainFields(tail, parent))
        }
      }

      private def parse(obj: JsObject, parent: JsPath): JsResult[Any] = {
        if (obj.fields.length > 0) {
          obj.fields(0) match {
            case ("$date", v: JsValue) =>
              val path = parent \ "$date"
              try {
                v match {
                  case number: JsNumber => JsSuccess(new Date(number.value.toLong), path)
                  case _                => JsSuccess(DateFormat.getDateInstance().parse(v.toString))
                }
              } catch {
                case ex: IllegalArgumentException => JsError(path, ValidationError("validation.invalid", "$date"))
              }
            case ("$oid", v: JsString) =>
              val path = parent \ "$oid"
              try {
                JsSuccess(new ObjectId(v.value), path)
              } catch {
                case ex: IllegalArgumentException => JsError(path, ValidationError("validation.invalid", "$oid"))
              }
            case _ => parsePlainObject(obj, parent)
          }
        } else parsePlainObject(obj, parent)
      }

      private def parse(arr: JsArray, parent: JsPath): JsResult[List[Any]] = {
        parse(arr.value.toList, parent, 0)
      }

      private def parse(l: List[JsValue], parent: JsPath, i: Int): JsResult[List[Any]] = {
        l match {
          case Nil          => JsSuccess(Nil)
          case head :: tail => cons(parse(head, parent(i)), parse(tail, parent, i + 1))
        }
      }

      private def cons[T](head: JsResult[T], tail: JsResult[List[T]]): JsResult[List[T]] = {
        (head, tail) match {
          case (h: JsError, t: JsError)           => h ++ t
          case (JsSuccess(h, _), JsSuccess(t, _)) => JsSuccess(h :: t)
          case (h: JsError, _)                    => h
          case _                                  => tail
        }
      }

      private def parse(js: JsValue, parent: JsPath): JsResult[Any] = {
        js match {
          case v: JsObject    => parse(v, parent)
          case v: JsArray     => parse(v, parent)
          case v: JsString    => JsSuccess(v.value, parent)
          // unless we call .toLong, we'll encode as BigDecimal and Casbah will throw serialization exception
          case v: JsNumber    => JsSuccess(v.value.toLong, parent)
          case v: JsBoolean   => JsSuccess(v.value, parent)
          case JsNull         => JsSuccess(null)
          case _: JsUndefined => JsSuccess(null)
        }
      }
    }
  }

  /*
   * Translate path to MongoDB path using schema
   *
   * e.g.
   *  /           -> content
   *  /securities/ -> content.securities
   *  /securities/sharesOwnedFollowingTransaction/ ->
   *    content.securities.content.sharesOwnedFollowingTransaction
   */
  def mongoDBPath(path: Path): String = path.dropRight(1).replaceFirst("/", "content.").replaceAll("/", ".content.")
}