import scala.collection.mutable

class Environment(enclosing: Option[Environment] = None) {
  private val values = new mutable.HashMap[String, Option[Any]]()

  def define(name: String, value: Option[Any]): Unit =
    values.put(name, value)

  def get(name: Token): Option[Any] = values.get(name.lexeme) match {
    case None =>
      enclosing match {
        case None      => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
        case Some(env) => env.get(name)
      }
    case Some(value) => value
  }

  def assign(name: Token, value: Option[Any]): Unit = {
    if (values.contains(name.lexeme)) {
      values.update(name.lexeme, value)
    } else {
      enclosing match {
        case None      => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
        case Some(env) => env.assign(name, value)
      }
    }
  }
}
