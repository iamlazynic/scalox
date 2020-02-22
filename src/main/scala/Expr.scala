sealed trait Expr {
  override def toString: String = {
    this match {
      case Assign(name, value)       => s"(assign ${name.lexeme} $value)"
      case Binary(left, op, right)   => parenthesize(op.lexeme, left, right)
      case Call(callee, paren, args) => s"(apply $callee (${args.mkString(",")}))"
      case Grouping(expr)            => parenthesize("group", expr)
      case Lambda(params, body)      => s"<lambda (${params.length})>"
      case Literal(value)            => value.toString
      case Unary(op, right)          => parenthesize(op.lexeme, right)
      case Variable(name)            => s"(var ${name.lexeme})"
    }
  }

  private def parenthesize(name: String, expressions: Expr*): String =
    s"($name ${expressions.mkString(" ")})"
}

case class Assign(name: Token, value: Expr)                     extends Expr
case class Binary(left: Expr, op: Token, right: Expr)           extends Expr
case class Call(callee: Expr, paren: Token, args: Vector[Expr]) extends Expr
case class Grouping(expr: Expr)                                 extends Expr
case class Lambda(params: Vector[Token], body: Vector[Stmt])    extends Expr
case class Literal(value: Terminal)                             extends Expr
case class Unary(op: Token, right: Expr)                        extends Expr
case class Variable(name: Token)                                extends Expr
