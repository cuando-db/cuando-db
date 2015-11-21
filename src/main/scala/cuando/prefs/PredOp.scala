package cuando.prefs

import play.api.libs.json.JsValue
import play.api.libs.json.Reads
import play.api.libs.json.JsObject
import play.api.libs.json.JsResult
import com.sun.org.apache.xpath.internal.operations.Lt
import play.api.libs.json.JsString
import play.api.libs.json.JsSuccess
import play.api.libs.json.Writes
import play.api.libs.json.Json

/*
 * Implementation based on Scala Cookbook section 6.9
 */
trait PredOp {
  def eval[T <% Ordered[T]](a: T, b: T): Boolean

  override def equals(other: Any) = other match {
    case that: PredOp => this.getClass.getName == that.getClass.getName
    case _            => false
  }
}

object PredOp {

  private class Lt extends PredOp {
    override def eval[T <% Ordered[T]](a: T, b: T): Boolean = a < b
    override def toString = "<"
  }
  private class Leq extends PredOp {
    override def eval[T <% Ordered[T]](a: T, b: T): Boolean = a <= b
    override def toString = "<="
  }
  private class Gt extends PredOp {
    override def eval[T <% Ordered[T]](a: T, b: T): Boolean = a > b
    override def toString = ">"
  }
  private class Geq extends PredOp {
    override def eval[T <% Ordered[T]](a: T, b: T): Boolean = b <= a
    override def toString = ">="
  }
  private class Eq extends PredOp {
    override def eval[T <% Ordered[T]](a: T, b: T): Boolean = a == b
    override def toString = "=="
  }
  private class Neq extends PredOp {
    override def eval[T <% Ordered[T]](a: T, b: T): Boolean = a != b
    override def toString = "!="
  }

  def apply(opName: String): PredOp = opName match {
    case "<"  => new Lt
    case "<=" => new Leq
    case ">"  => new Gt
    case ">=" => new Geq
    case "==" => new Eq
    case "!=" => new Neq
    case _    => throw new NotImplementedError("unknown op %s".format(opName))
  }

  implicit val predOpReads: Reads[PredOp] = new Reads[PredOp] {
    def reads(json: JsValue): JsResult[PredOp] = {
      val predOp = json match {
        case JsString(opStr) => PredOp(opStr)
        case _               => throw new IllegalArgumentException("%s operator undefined".format(json))
      }
      JsSuccess(predOp)
    }
  }

  implicit val predOpWrites: Writes[PredOp] = new Writes[PredOp] {
    def writes(predOp: PredOp): JsString = JsString(predOp.toString)
  }
}
