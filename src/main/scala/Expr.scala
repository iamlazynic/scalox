sealed trait Expr {
  override def toString: String = {
    this match {
      case Binary(left, op, right) => parenthesize(op.lexeme, left, right)
      case Grouping(expr)          => parenthesize("group", expr)
      case Literal(value)          => value match { case None => "nil"; case Some(v) => v.toString }
      case Unary(op, right)        => parenthesize(op.lexeme, right)
    }
  }

  private def parenthesize(name: String, expressions: Expr*): String =
    s"($name ${expressions.mkString(" ")})"
}

case class Binary(left: Expr, op: Token, right: Expr) extends Expr
case class Grouping(expr: Expr)                       extends Expr
case class Literal(valu: Option[Any])                 extends Expr
case class Unary(op: Token, right: Expr)              extends Expr
