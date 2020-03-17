object Expr {
  private var i = -1

  def index: Int = {
    i += 1
    i
  }
}

sealed trait Expr {
  override def toString: String = {
    this match {
      case Assign(name, value, _)       => s"(assign ${name.lexeme} $value)"
      case Binary(left, op, right, _)   => parenthesize(op.lexeme, left, right)
      case Call(callee, paren, args, _) => s"(apply $callee (${args.mkString(",")}))"
      case Grouping(expr, _)            => parenthesize("group", expr)
      case Lambda(params, body, _)      => s"<lambda (${params.length})>"
      case Literal(value, _)            => value.toString
      case Unary(op, right, _)          => parenthesize(op.lexeme, right)
      case Variable(name, _)            => s"(var ${name.lexeme})"
    }
  }

  private def parenthesize(name: String, expressions: Expr*): String =
    s"($name ${expressions.mkString(" ")})"
}

case class Assign(name: Token, value: Expr, index: Int) extends Expr

case class Binary(left: Expr, op: Token, right: Expr, index: Int) extends Expr

case class Call(callee: Expr, paren: Token, args: Vector[Expr], index: Int) extends Expr

case class Grouping(expr: Expr, index: Int) extends Expr

case class Lambda(params: Vector[Token], body: Vector[Stmt], index: Int) extends Expr

case class Literal(value: Terminal, index: Int) extends Expr

case class Unary(op: Token, right: Expr, index: Int) extends Expr

case class Variable(name: Token, index: Int) extends Expr
