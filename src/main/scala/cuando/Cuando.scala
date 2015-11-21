package cuando

import java.io.File

import com.mongodb.casbah.MongoClient

import cuando.prefs.PreferenceRules
import cuando.time.TemporalDimensions
import cuando.types.Schema
import play.api.libs.json.Json

object Cuando extends App {

  case class Config(call: String = "",
                    archiveName: String = "",
                    schema: File = new File(""),
                    prefs: File = new File(""),
                    dims: File = new File(""),
                    doc: File = new File(""),
                    query: File = new File(""))

  val parser = new scopt.OptionParser[Config]("cuando") {
    head("cuando command-line client", "1.0")
    help("help")

    cmd("init") action { (_, c) =>
      c.copy(call = "init")
    } text ("Initialize an archive.") children (
      arg[String]("<archive>") required () action { (x, c) =>
        c.copy(archiveName = x)
      } text ("archive name required"),
      arg[File]("<schema>") required () action { (x, c) =>
        c.copy(schema = x)
      } text ("schema required"),
      arg[File]("<prefs>") required () action { (x, c) =>
        c.copy(prefs = x)
      } text ("prefs required"),
      arg[File]("<dims>") required () action { (x, c) =>
        c.copy(dims = x)
      } text ("dims required"))

    cmd("merge") action { (_, c) =>
      c.copy(call = "merge")
    } text ("Merge a document into an archive.") children (
      arg[String]("<archive>") required () action { (x, c) =>
        c.copy(archiveName = x)
      } text ("archive name required"),
      arg[File]("<doc>") required () action { (x, c) =>
        c.copy(doc = x)
      } text ("document required"))

    cmd("query") action { (_, c) =>
      c.copy(call = "query")
    } text ("Query an archive.") children (
      arg[String]("<archive>") required () action { (x, c) =>
        c.copy(archiveName = x)
      } text ("archive name required"),
      arg[File]("<query>") required () action { (x, c) =>
        c.copy(query = x)
      } text ("query required"))
  }
  // parser.parse returns Option[C]
  parser.parse(args, Config()) match {
    case Some(config) =>
      val mongoClient = MongoClient("localhost", 27017)
      val cuando = CuandoAPI(mongoClient)

      config.call match {
        case "init" => {
          val schema = Json.parse(scala.io.Source.fromFile(config.schema).mkString).as[Schema]
          val prefs = Json.parse(scala.io.Source.fromFile(config.prefs).mkString).as[PreferenceRules]
          val dims = Json.parse(scala.io.Source.fromFile(config.dims).mkString).as[TemporalDimensions]
          val res = cuando.init(config.archiveName, schema, dims, prefs)
          println(StringContext treatEscapes res.toString)
        }

        case "merge" => {
          val doc = Json.parse(scala.io.Source.fromFile(config.doc).mkString)
          val res = cuando.merge(config.archiveName, doc)
          println(StringContext treatEscapes res.toString)
        }

        case "query" => {
          val query = Json.parse(scala.io.Source.fromFile(config.query).mkString)
          val sc = (query \ "select").asOpt[SelectClause]
          val pc = (query \ "project").asOpt[ProjectClause]
          val tc = (query \ "timeslice").asOpt[TimeSliceClause]
          (sc, pc, tc) match {
            case (None, _, _) => throw new IllegalArgumentException("Select clause must be defined.")
            case _ => {
              val res = cuando.query(config.archiveName, sc.get, pc, tc)
              println(StringContext treatEscapes res.toString)
            }
          }
        }
        case "" => parser.showUsage
      }
    case None =>
    // arguments are bad, error message will have been displayed
  }
}