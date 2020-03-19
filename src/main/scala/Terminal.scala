import scala.collection.mutable

sealed trait Terminal {
  override def toString: String = this match {
    case TNumber(value) =>
      val s = value.toString
      if (s.endsWith(".0")) s.substring(0, s.length - 2) else s
    case TString(value) => value
    case TBoolean(value) => value.toString
    case TNative(arity, _) => s"native function ($arity) {...}"
    case TFunction(params, _, _, _) => s"fun (${params.length}  {...})"
    case TClass(name, _, _, _, _) => s"class $name"
    case TInstance(klass, _) => s"${klass.name} instance"
    case TNil() => "nil"
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
case class TFunction(params: Vector[Token], body: Vector[Stmt], env: Environment, isInitializer: Boolean)
    extends Terminal
case class TNative(arity: Int, func: Vector[Terminal] => Terminal) extends Terminal
case class TClass(name: String,
                  superclass: Option[TClass],
                  staticMethods: mutable.HashMap[String, TFunction],
                  methods: mutable.HashMap[String, TFunction],
                  index: Int)
    extends Terminal {
  def method(name: String): Option[TFunction] = {
    methods.get(name) match {
      case Some(method) => Some(method)
      case None =>
        superclass match {
          case Some(klass) => klass.method(name)
          case None        => None
        }
    }
  }
  def staticMethod(name: String): Option[TFunction] = {
    staticMethods.get(name) match {
      case Some(method) => Some(method)
      case None =>
        superclass match {
          case Some(klass) => klass.staticMethod(name)
          case None        => None
        }
    }
  }
}
case class TInstance(klass: TClass, index: Int) extends Terminal {
  val fields = new mutable.HashMap[String, Terminal]()
}
case class TNil() extends Terminal
