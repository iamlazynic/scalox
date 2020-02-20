sealed trait Stmt {}

case class Block(statements: Array[Stmt])                                 extends Stmt
case class Break()                                                        extends Stmt
case class Expression(expr: Expr)                                         extends Stmt
case class Function(name: Token, params: Array[Token], body: Array[Stmt]) extends Stmt
case class If(cond: Expr, brThen: Stmt, brElse: Option[Stmt])             extends Stmt
case class Print(expr: Expr)                                              extends Stmt
case class Return(keyword: Token, value: Option[Expr])                    extends Stmt
case class Var(name: Token, initializer: Option[Expr])                    extends Stmt
case class While(cond: Expr, body: Stmt)                                  extends Stmt
