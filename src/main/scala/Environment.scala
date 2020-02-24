import scala.collection.mutable

class Environment(val enclosing: Option[Environment] = None) {
  private val values = new mutable.HashMap[String, Terminal]()

  def define(name: String, value: Terminal): Unit =
    values.put(name, value)

  def get(name: Token): Terminal = values.get(name.lexeme) match {
    case None =>
      enclosing match {
        case None      => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
        case Some(env) => env.get(name)
      }
    case Some(value) => value
  }

  def getAt(depth: Int, name: String): Terminal =
    ancestor(depth).values(name)

  def assign(name: Token, value: Terminal): Unit = {
    if (values.contains(name.lexeme)) {
      values.update(name.lexeme, value)
    } else {
      enclosing match {
        case None      => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
        case Some(env) => env.assign(name, value)
      }
    }
  }

  def assignAt(depth: Int, name: Token, value: Terminal): Unit =
    ancestor(depth).values.put(name.lexeme, value)

  private def ancestor(depth: Int): Environment = {
    var env = this
    for (_ <- 0 until depth) env = env.enclosing.get
    env
  }
}
