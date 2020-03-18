import scala.collection.mutable

sealed trait Terminal {
  override def toString: String = this match {
    case TNumber(value) =>
      val s = value.toString
      if (s.endsWith(".0")) s.substring(0, s.length - 2) else s
    case TString(value)                   => value
    case TBoolean(value)                  => value.toString
    case TFunction(params, body, func, _) => s"fun (${params.length}  {...})"
    case TClass(name, methods, _)         => s"class $name"
    case TInstance(klass, _)              => s"${klass.name} instance"
    case TNil()                           => "nil"
  }
}

object TClass {
  private var i = -1
  def index: Int = {
    i = i + 1
    i
  }
}

case class TNumber(value: Double)   extends Terminal
case class TString(value: String)   extends Terminal
case class TBoolean(value: Boolean) extends Terminal
case class TFunction(params: Vector[Token],
                     body: Vector[Stmt],
                     env: Environment,
                     isInitializer: Boolean)
    extends Terminal
case class TClass(name: String, methods: mutable.HashMap[String, TFunction], index: Int)
    extends Terminal
case class TInstance(klass: TClass, index: Int) extends Terminal {
  val fields = new mutable.HashMap[String, Terminal]()
}
case class TNil() extends Terminal
