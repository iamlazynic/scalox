sealed trait Expr {
  override def toString: String = {
    this match {
      case Assign(name, value)     => parenthesize(s"assign ${name.lexeme}", value)
      case Binary(left, op, right) => parenthesize(op.lexeme, left, right)
      case Grouping(expr)          => parenthesize("group", expr)
      case Literal(value)          => value match { case None => "nil"; case Some(v) => v.toString }
      case Unary(op, right)        => parenthesize(op.lexeme, right)
      case Variable(name)          => parenthesize(s"var ${name.lexeme}")
    }
  }

  private def parenthesize(name: String, expressions: Expr*): String =
    s"($name ${expressions.mkString(" ")})"
}

case class Assign(name: Token, value: Expr)           extends Expr
case class Binary(left: Expr, op: Token, right: Expr) extends Expr
case class Grouping(expr: Expr)                       extends Expr
case class Literal(value: Option[Any])                extends Expr
case class Unary(op: Token, right: Expr)              extends Expr
case class Variable(name: Token)                      extends Expr
