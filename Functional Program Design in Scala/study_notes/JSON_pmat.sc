abstract class JSON
case class JSeq (elems: List[JSON]) extends JSON
case class JObj (bindings: Map[String, JSON]) extends JSON
case class JNum (num: Double) extends JSON
case class JStr (str: String) extends JSON
case class JBool(b: Boolean) extends JSON
case object JNull extends JSON

def show(json: JSON): String = {
  def repr_str(str: String): String = "\"" + str + "\""

  json match {
    case JSeq(elems) => "[" + (elems map show mkString ", ") + "]"
    case JObj(bindings) =>
      val assocs = bindings map {case (key, value) => repr_str(key) + ": " + show(value)}
      "{" + (assocs mkString ", ") + "}"
    case JNum(num) => num.toString
    case JStr(str) => repr_str(str)
    case JBool(b) => b.toString
    case JNull => "null"
  }
}

val add_data = JObj(Map("streetAddress" -> JStr("21 2nd Street"), "state" -> JStr("NY"), "postalCode" -> JNum(10021)))

val phone_data = JSeq(List(
  JObj(Map("type" -> JStr("home"), "number" -> JStr("212 555-1234"))),
  JObj(Map("type" -> JStr("fax"), "number" -> JStr("646 555-4567")))))

val data = JObj(Map(
  "firstName" -> JStr("John"),
  "lastName" -> JStr("Smith"),
  "address" -> add_data,
  "phoneNumbers" -> phone_data))

show(add_data)
show(phone_data)
show(data)