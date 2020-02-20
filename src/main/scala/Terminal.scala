sealed trait Terminal {
  override def toString: String = this match {
    case TNumber(value) =>
      val s = value.toString
      if (s.endsWith(".0")) s.substring(0, s.length - 2) else s
    case TString(value)  => value
    case TBoolean(value) => value.toString
    case TNil()          => "nil"
  }
}

case class TNumber(value: Double)   extends Terminal
case class TString(value: String)   extends Terminal
case class TBoolean(value: Boolean) extends Terminal
case class TNil()                   extends Terminal
