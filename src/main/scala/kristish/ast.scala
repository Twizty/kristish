package kristish

object ast {
  sealed trait JsonValue

  object JsonValue {
    case class JsonObject(fields: Map[String, JsonValue]) extends JsonValue
    case class JsonArray(items: List[JsonValue]) extends JsonValue
    case class JsonString(value: String) extends JsonValue
    case class JsonNumber(value: Double) extends JsonValue
    case class JsonBoolean(value: Boolean) extends JsonValue
    case object JsonNull extends JsonValue
  }
}
