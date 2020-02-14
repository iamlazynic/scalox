sealed trait Expr {
  def parenthesize(name: String, expressions: Expr*): String =
    s"($name ${expressions.mkString(" ")})"
}

case class Binary(left: Expr, op: Token, right: Expr) extends Expr {
  override def toString: String = parenthesize(op.lexeme, left, right)
}

case class Grouping(expr: Expr) extends Expr {
  override def toString: String = parenthesize("group", expr)
}

case class Literal(value: Option[Any]) extends Expr {
  override def toString: String = value match {
    case None    => "nil"
    case Some(v) => v.toString
  }
}

case class Unary(op: Token, right: Expr) extends Expr {
  override def toString: String = parenthesize(op.lexeme, right)
}
