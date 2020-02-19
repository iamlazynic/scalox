class Interpreter {
  val top = new Environment()

  def interpret(item: Either[Array[Stmt], Expr]): Unit = {
    try {
      item match {
        case Left(statements) => statements.foreach(execute(top))
        case Right(expr)      => println(stringify(evaluate(top)(expr)))
      }
    } catch {
      case e: RuntimeError => Lox.error(e)
    }
  }

  private def execute(env: Environment)(stmt: Stmt): Unit = stmt match {
    case Block(statements) =>
      val local = new Environment(Some(env))
      statements.foreach(execute(local))
    case Expression(expr) => evaluate(env)(expr)
    case Print(expr)      => println(stringify(evaluate(env)(expr)))
    case Var(name, initializer) =>
      val value = initializer match {
        case None       => None
        case Some(expr) => evaluate(env)(expr)
      }
      env.define(name.lexeme, value)
  }

  private def evaluate(env: Environment)(expr: Expr): Option[Any] = expr match {
    case Assign(name, value) =>
      val valu = evaluate(env)(value)
      env.assign(name, valu)
      valu
    case Binary(left, op, right) =>
      val lv    = evaluate(env)(left)
      val rv    = evaluate(env)(right)
      val strip = tok => stripNumber(op, tok)
      op.typ match {
        case TokenType.MINUS => Some(strip(lv) - strip(rv))
        case TokenType.PLUS =>
          (lv, rv) match {
            case (Some(ld: Double), Some(rd: Double)) => Some(ld + rd)
            case (Some(ls: String), Some(rs: String)) => Some(ls + rs)
            case (Some(ls: String), Some(rd: Double)) => Some(ls + rd)
            case _ =>
              val msg =
                "Operands must be pair (number, number), (string, string), or (string, number)."
              throw RuntimeError(op, msg)
          }
        case TokenType.STAR          => Some(strip(lv) * strip(rv))
        case TokenType.SLASH         => Some(strip(lv) / strip(rv))
        case TokenType.GREATER       => Some(strip(lv) > strip(rv))
        case TokenType.GREATER_EQUAL => Some(strip(lv) >= strip(rv))
        case TokenType.LESS          => Some(strip(lv) < strip(rv))
        case TokenType.LESS_EQUAL    => Some(strip(lv) <= strip(rv))
        case TokenType.BANG_EQUAL    => Some(lv != rv)
        case TokenType.EQUAL_EQUAL   => Some(lv == rv)
        case TokenType.COMMA         => rv
        // TODO: exhausted match?
      }
    case Grouping(expr) => evaluate(env)(expr)
    case Literal(value) => value
    case Unary(op, right) =>
      val rv = evaluate(env)(right)
      op.typ match {
        case TokenType.MINUS => Some(-stripNumber(op, rv))
        case TokenType.BANG  => Some(!isTruthy(rv))
        // TODO: exhausted match?
      }
    case Variable(name) => env.get(name)
  }

  private def stripNumber(operator: Token, operand: Option[Any]): Double = operand match {
    case Some(d: Double) => d
    case _               => throw RuntimeError(operator, "Operand must be a number.")
  }

  private def isTruthy(x: Option[Any]): Boolean = x match {
    case None        => false
    case Some(false) => false
    case _           => true
  }

  private def stringify(value: Option[Any]): String = value match {
    case None    => "nil"
    case Some(v) => v.toString
  }
}
